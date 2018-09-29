open Debug_protocol

type t = {
  in_chan : Lwt_io.input_channel;
  out_chan : Lwt_io.output_channel;
  mutable next_seq : int;
  command_handler_tbl : (string, t -> Request.t -> Response.t Lwt.t) Hashtbl.t;
  event_handler_tbl : (string, t -> Event.t -> unit Lwt.t) Hashtbl.t;
  resolver_tbl : (int, Response.t Lwt.u) Hashtbl.t;
}

exception Protocol_error

let create in_chan out_chan =
  { in_chan; out_chan;
    next_seq = 0;
    command_handler_tbl = Hashtbl.create 0;
    event_handler_tbl = Hashtbl.create 0;
    resolver_tbl = Hashtbl.create 0 }

let close rpc =
  Lwt_io.close rpc.in_chan;%lwt
  Lwt_io.close rpc.out_chan

let get_ok result =
  match result with
  | Ok result -> result
  | Error _ -> invalid_arg "get_ok"

let new_seq rpc =
  let seq = rpc.next_seq in
  rpc.next_seq <- rpc.next_seq + 1;
  seq

let send_message rpc msg  =
  let msg_str = Yojson.Safe.to_string msg in
  (* print_endline ("<<< " ^ msg_str); *)
  Lwt_io.write rpc.out_chan (
    "Content-Length: " ^
    string_of_int (String.length msg_str) ^
    "\r\n\r\n" ^
    msg_str
  )

let emit_event (rpc : t)
    (type body)
    (module Event' : EVENT with type Body.t = body)
    (body : body) : unit Lwt.t =
  send_message rpc Event.({
    seq = new_seq rpc;
    type_ = "event";
    event = Event'.name;
    body = Event'.Body.to_yojson body;
  } |> to_yojson)

let exec_command (rpc : t)
    (type args) (type body)
    (module Command : COMMAND with type Request.Arguments.t = args
                               and type Response.Body.t = body)
    (args : args) : (body, string * Message.t option) result Lwt.t =
  let promise, resolver = Lwt.wait () in
  let request_seq = new_seq rpc in
  Hashtbl.replace rpc.resolver_tbl request_seq resolver;
  send_message rpc Request.(
    make ~seq:request_seq ~type_:"request" ~command:Command.name
      ~arguments:(Command.Request.Arguments.to_yojson args) () |> to_yojson);%lwt
  let%lwt resp = promise in
  Hashtbl.remove rpc.resolver_tbl request_seq;
  if resp.success then (
    Lwt.return_ok (Command.Response.Body.of_yojson resp.body |> get_ok)
  ) else (
    let body = ErrorResponse.Body.of_yojson resp.body |> get_ok in
    Lwt.return_error (BatOption.get resp.message, body.error)
  )

let handle_event (rpc : t)
    (type body)
    (module Event : EVENT with type Body.t = body)
    (handler : body -> unit Lwt.t) : unit =
  Hashtbl.add rpc.event_handler_tbl Event.name (fun _ evt ->
    let body = Event.Body.of_yojson evt.body |> get_ok in
    handler body
  )

let handle_command (rpc : t)
    (type args) (type body)
    (module Command : COMMAND with type Request.Arguments.t = args
                               and type Response.Body.t = body)
    (handler : args -> (body, string * Message.t option) result Lwt.t) : unit =
  assert (not (Hashtbl.mem rpc.command_handler_tbl Command.name));
  Hashtbl.add rpc.command_handler_tbl Command.name (fun rpc req ->
    let args = Command.Request.Arguments.of_yojson req.arguments |> get_ok in
    let%lwt success, message, body =
      match%lwt handler args with
      | Ok body -> Lwt.return (true, None, Command.Response.Body.to_yojson body)
      | Error (message, error) -> Lwt.return (false, Some message, ErrorResponse.Body.({error} |> to_yojson))
    in
    Lwt.return Response.(
      make ~seq:(new_seq rpc) ~type_:"response" ~request_seq:req.seq
        ~success ~command:req.command ~message ~body ())
  )

let message =
  let open Angstrom in
  let eol = string "\r\n" in
  let colon = string ": " in
  let is_colon = function ':' -> true | _ -> false in
  let is_eol = function '\r' -> true | _ -> false in
  let header_field = lift2
      (fun key value -> key, value)
      (take_till is_colon <* colon)
      (take_till is_eol <* eol) in
  (many1 header_field <* eol) >>= fun headers ->
  let content_length = int_of_string (List.assoc "Content-Length" headers) in
  take content_length >>| fun content -> Yojson.Safe.from_string content

let start (rpc : t) : unit Lwt.t =
  let dispatch_event msg =
    let evt = Event.of_yojson msg |> get_ok in
    let handler = Hashtbl.find rpc.event_handler_tbl evt.event in
    handler rpc evt
  in
  let dispatch_request msg =
    let req = Request.of_yojson msg |> get_ok in
    let handler = Hashtbl.find rpc.command_handler_tbl req.command in
    let%lwt resp = handler rpc req in
    send_message rpc (Response.to_yojson resp)
  in
  let dispatch_response msg =
    let resp = Response.of_yojson msg |> get_ok in
    let resolver = Hashtbl.find rpc.resolver_tbl resp.request_seq in
    Lwt.wakeup_later resolver resp;
    Lwt.return_unit
  in
  let dispatch msg =
    (* print_endline (">>> " ^ Yojson.Safe.to_string msg); *)
    match Protocol_message.of_yojson msg |> get_ok with
    | {type_ = "event"; _} -> dispatch_event msg
    | {type_ = "request"; _} -> dispatch_request msg
    | {type_ = "response"; _} -> dispatch_response msg
    | _ -> raise Protocol_error
  in
  try%lwt (
    let%lwt _ = Angstrom_lwt_unix.parse_many message dispatch rpc.in_chan in
    Lwt.return_unit
  ) with Lwt_io.Channel_closed _ -> Lwt.return ()
