(* Stop inside a closure and look at the Heap scope, which holds the variables
   the closure captured from its enclosing scope rather than the ones on its own
   stack.

   This is the scope read by Value_scope.iter_compenv_heap, whose shape changed
   with the compiler (Instruct.compilation_env grew a ce_closure field), so it
   is version-sensitive in the same way the Globals scope was in #74. *)

open Debug_protocol

let print_scope name variables =
  let variables =
    variables
    |> List.map (fun (variable : Variable.t) ->
           Printf.sprintf "%s = %s" variable.name variable.value)
  in
  match variables with
  | [] -> Printf.printf "%s: <empty>\n" name
  | variables -> Printf.printf "%s: %s\n" name (String.concat ", " variables)

let main () =
  (* Line 9 is [let bumped = !count + step in], inside [bump]: [count] and
     [step] are captured, [bumped] is local. *)
  Dap_client.with_session ~program:"fixtures/closure.bc"
    ~source:"fixtures/closure.ml" ~breakpoints:[ 9 ] (fun t ->
      let%lwt stopped = Dap_client.wait_stopped t in
      let thread_id = Option.value stopped.thread_id ~default:0 in
      let%lwt frame = Dap_client.top_frame t ~thread_id in
      Printf.printf "stopped in %s:%d\n" frame.name frame.line;
      let%lwt stack = Dap_client.scope t ~frame ~name:"Stack" in
      print_scope "Stack" stack;
      let%lwt heap = Dap_client.scope t ~frame ~name:"Heap" in
      print_scope "Heap" heap;
      (* [count] is a ref, so it expands to its contents. *)
      heap
      |> Lwt_list.iter_s (fun (variable : Variable.t) ->
             let%lwt children = Dap_client.expand t ~variable in
             children
             |> List.iter (fun (child : Variable.t) ->
                    Printf.printf "  %s.%s = %s\n" variable.name child.name
                      child.value);
             Lwt.return ()))

let () = Lwt_main.run (main ())
