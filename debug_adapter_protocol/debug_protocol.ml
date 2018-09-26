[@@@warning "-39"]

module type YOJSON_TYPE = sig
  type t

  val of_yojson : Yojson.Safe.json -> t Ppx_deriving_yojson_runtime.error_or
  val to_yojson : t -> Yojson.Safe.json
end

module type EVENT = sig
  val name : string

  module Body : YOJSON_TYPE
end

module type COMMAND = sig
  val name : string

  module Request : sig
    module Arguments : YOJSON_TYPE
  end

  module Response : sig
    module Body : YOJSON_TYPE
  end
end

module Int_or_string = struct
  type t =
    | Int of int
    | String of string

  let of_yojson = function
    | `Int value -> Ok (Int value)
    | `String value -> Ok (String value)
    | _ -> Error "Yojson_util.Int_or_string.t"

  let to_yojson = function
    | Int value -> `Int value
    | String value -> `String value
end

module Blank = struct
  type t = unit
  let of_yojson = function
    | `Assoc [] -> Ok ()
    | _ -> Error "Yojson_util.Blank.t"
  let to_yojson () = `Assoc []
end

module Make_dict (P : YOJSON_TYPE) = struct
  module StringMap = Map.Make(String)

  type t = P.t StringMap.t

  let empty = StringMap.empty
  let is_empty = StringMap.is_empty
  let mem = StringMap.mem
  let add = StringMap.add
  let singleton = StringMap.singleton
  let remove = StringMap.remove
  let merge = StringMap.merge
  let union = StringMap.union
  let compare = StringMap.compare
  let equal = StringMap.equal
  let iter = StringMap.iter
  let fold = StringMap.fold
  let for_all = StringMap.for_all
  let exists = StringMap.exists
  let filter = StringMap.filter
  let partition = StringMap.partition
  let cardinal = StringMap.cardinal
  let bindings = StringMap.bindings
  let min_binding = StringMap.min_binding
  let max_binding = StringMap.max_binding
  let choose = StringMap.choose
  let split = StringMap.split
  let find = StringMap.find
  let map = StringMap.map
  let mapi = StringMap.mapi

  let of_yojson json =
    match (
      match json with
      | `Assoc l ->
        let rec build map = function
          | [] -> map
          | (k, v) :: tl -> (
              match P.of_yojson v with
              | Ok value -> build (add k value map) tl
              | Error msg -> failwith msg
            )
        in build empty l
      | _ -> failwith "Yojson_util.Make_dict.t"
    ) with
    | exception Failure msg -> Result.Error msg
    | t -> Ok t

  let to_yojson dict =
    `Assoc (fold (fun k v l -> (k, P.to_yojson v) :: l) dict [])
end

module String_dict = Make_dict (struct
    type t = string [@@deriving yojson]
  end)

module String_opt_dict = Make_dict (struct
    type t = string option [@@deriving yojson]
  end)
module Exception_breakpoints_filter = struct
  type t = {
    filter : string;
    label : string;
    default : bool [@default false];
  } [@@deriving make, yojson { strict = false }]
end

module Column_descriptor = struct
  module Type = struct
    type t =
        String
      | Number
      | Boolean
      | Unix_timestamp_utc
    let of_yojson = function
      | `String "string" -> Ok String
      | `String "number" -> Ok Number
      | `String "boolean" -> Ok Boolean
      | `String "unixTimestampUTC" -> Ok Unix_timestamp_utc
      | _ -> Error "Debug_protocol.ColumnDescriptor.Type.t"

    let to_yojson = function
      | String -> `String "string"
      | Number -> `String "number"
      | Boolean -> `String "boolean"
      | Unix_timestamp_utc -> `String "unixTimestampUTC"
  end

  type t = {
    attribute_name : string [@key "attributeName"];
    label : string;
    format : string option [@default None];
    type_ : Type.t option [@key "type"] [@default None];
    width : int option [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Checksum_algorithm = struct
  type t =
      MD5
    | SHA1
    | SHA256
    | Timestamp
  let of_yojson = function
    | `String "MD5" -> Ok MD5
    | `String "SHA1" -> Ok SHA1
    | `String "SHA256" -> Ok SHA256
    | `String "timestamp" -> Ok Timestamp
    | _ -> Error "Debug_protocol.ChecksumAlgorithm.t"

  let to_yojson = function
    | MD5 -> `String "MD5"
    | SHA1 -> `String "SHA1"
    | SHA256 -> `String "SHA256"
    | Timestamp -> `String "timestamp"
end

module Capabilities = struct
  type t = {
    supports_configuration_done_request : bool [@key "supportsConfigurationDoneRequest"] [@default false];
    supports_function_breakpoints : bool [@key "supportsFunctionBreakpoints"] [@default false];
    supports_conditional_breakpoints : bool [@key "supportsConditionalBreakpoints"] [@default false];
    supports_hit_conditional_breakpoints : bool [@key "supportsHitConditionalBreakpoints"] [@default false];
    supports_evaluate_for_hovers : bool [@key "supportsEvaluateForHovers"] [@default false];
    exception_breakpoint_filters : Exception_breakpoints_filter.t list [@key "exceptionBreakpointFilters"] [@default []];
    supports_step_back : bool [@key "supportsStepBack"] [@default false];
    supports_set_variable : bool [@key "supportsSetVariable"] [@default false];
    supports_restart_frame : bool [@key "supportsRestartFrame"] [@default false];
    supports_goto_targets_request : bool [@key "supportsGotoTargetsRequest"] [@default false];
    supports_step_in_targets_request : bool [@key "supportsStepInTargetsRequest"] [@default false];
    supports_completions_request : bool [@key "supportsCompletionsRequest"] [@default false];
    supports_modules_request : bool [@key "supportsModulesRequest"] [@default false];
    additional_module_columns : Column_descriptor.t list [@key "additionalModuleColumns"] [@default []];
    supported_checksum_algorithms : Checksum_algorithm.t list [@key "supportedChecksumAlgorithms"] [@default []];
    supports_restart_request : bool [@key "supportsRestartRequest"] [@default false];
    supports_exception_options : bool [@key "supportsExceptionOptions"] [@default false];
    supports_value_formatting_options : bool [@key "supportsValueFormattingOptions"] [@default false];
    supports_exception_info_request : bool [@key "supportsExceptionInfoRequest"] [@default false];
    support_terminate_debuggee : bool [@key "supportTerminateDebuggee"] [@default false];
    supports_delayed_stack_trace_loading : bool [@key "supportsDelayedStackTraceLoading"] [@default false];
    supports_loaded_sources_request : bool [@key "supportsLoadedSourcesRequest"] [@default false];
    supports_log_points : bool [@key "supportsLogPoints"] [@default false];
    supports_terminate_threads_request : bool [@key "supportsTerminateThreadsRequest"] [@default false];
    supports_set_expression : bool [@key "supportsSetExpression"] [@default false];
    supports_terminate_request : bool [@key "supportsTerminateRequest"] [@default false];
  } [@@deriving make, yojson { strict = false }]
end

module Message = struct
  type t = {
    id : int;
    format : string;
    variables : String_dict.t [@default String_dict.empty];
    send_telemetry : bool [@key "sendTelemetry"] [@default false];
    show_user : bool [@key "showUser"] [@default false];
    url : string option [@default None];
    url_label : string option [@key "urlLabel"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Module = struct
  type t = {
    id : Int_or_string.t;
    name : string;
    path : string option [@default None];
    is_optimized : bool [@key "isOptimized"] [@default false];
    is_user_code : bool [@key "isUserCode"] [@default false];
    version : string option [@default None];
    symbol_status : string option [@key "symbolStatus"] [@default None];
    symbol_file_path : string option [@key "symbolFilePath"] [@default None];
    date_time_stamp : string option [@key "dateTimeStamp"] [@default None];
    address_range : string option [@key "addressRange"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Modules_view_descriptor = struct
  type t = {
    columns : Column_descriptor.t list;
  } [@@deriving make, yojson { strict = false }]
end

module Thread = struct
  type t = {
    id : int;
    name : string;
  } [@@deriving make, yojson { strict = false }]
end

module Checksum = struct
  type t = {
    algorithm : Checksum_algorithm.t;
    checksum : string;
  } [@@deriving make, yojson { strict = false }]
end

module Source = struct
  module Presentation_hint = struct
    type t =
        Normal
      | Emphasize
      | Deemphasize
    let of_yojson = function
      | `String "normal" -> Ok Normal
      | `String "emphasize" -> Ok Emphasize
      | `String "deemphasize" -> Ok Deemphasize
      | _ -> Error "Debug_protocol.Source.Presentation_hint.t"

    let to_yojson = function
      | Normal -> `String "normal"
      | Emphasize -> `String "emphasize"
      | Deemphasize -> `String "deemphasize"
  end

  type t = {
    name : string option [@default None];
    path : string option [@default None];
    source_reference : int option [@key "sourceReference"] [@default None];
    presentation_hint : Presentation_hint.t option [@key "presentationHint"] [@default None];
    origin : string option [@default None];
    sources : t list [@default []];
    adapter_data : Yojson.Safe.json [@key "adapterData"] [@default `Assoc []];
    checksums : Checksum.t list [@default []];
  } [@@deriving make, yojson { strict = false }]
end

module Stack_frame = struct
  module Presentation_hint = struct
    type t =
        Normal
      | Label
      | Subtle
    let of_yojson = function
      | `String "normal" -> Ok Normal
      | `String "label" -> Ok Label
      | `String "subtle" -> Ok Subtle
      | _ -> Error "Debug_protocol.StackFrame.Presentation_hint.t"

    let to_yojson = function
      | Normal -> `String "normal"
      | Label -> `String "label"
      | Subtle -> `String "subtle"
  end

  type t = {
    id : int;
    name : string;
    source : Source.t option [@default None];
    line : int;
    column : int;
    end_line : int option [@key "endLine"] [@default None];
    end_column : int option [@key "endColumn"] [@default None];
    module_id : Int_or_string.t option [@key "moduleId"] [@default None];
    presentation_hint : Presentation_hint.t option [@key "presentationHint"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Scope = struct
  type t = {
    name : string;
    variables_reference : int [@key "variablesReference"];
    named_variables : int option [@key "namedVariables"] [@default None];
    indexed_variables : int option [@key "indexedVariables"] [@default None];
    expensive : bool;
    source : Source.t option [@default None];
    line : int option [@default None];
    column : int option [@default None];
    end_line : int option [@key "endLine"] [@default None];
    end_column : int option [@key "endColumn"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Variable_presentation_hint = struct
  type t = {
    kind : string option [@default None];
    attributes : string list [@default []];
    visibility : string option [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Variable = struct
  type t = {
    name : string;
    value : string;
    type_ : string option [@key "type"] [@default None];
    presentation_hint : Variable_presentation_hint.t option [@key "presentationHint"] [@default None];
    evaluate_name : string option [@key "evaluateName"] [@default None];
    variables_reference : int [@key "variablesReference"];
    named_variables : int option [@key "namedVariables"] [@default None];
    indexed_variables : int option [@key "indexedVariables"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Source_breakpoint = struct
  type t = {
    line : int;
    column : int option [@default None];
    condition : string option [@default None];
    hit_condition : string option [@key "hitCondition"] [@default None];
    log_message : string option [@key "logMessage"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Function_breakpoint = struct
  type t = {
    name : string;
    condition : string option [@default None];
    hit_condition : string option [@key "hitCondition"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Breakpoint = struct
  type t = {
    id : int option [@default None];
    verified : bool;
    message : string option [@default None];
    source : Source.t option [@default None];
    line : int option [@default None];
    column : int option [@default None];
    end_line : int option [@key "endLine"] [@default None];
    end_column : int option [@key "endColumn"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Step_in_target = struct
  type t = {
    id : int;
    label : string;
  } [@@deriving make, yojson { strict = false }]
end

module Goto_target = struct
  type t = {
    id : int;
    label : string;
    line : int;
    column : int option [@default None];
    end_line : int option [@key "endLine"] [@default None];
    end_column : int option [@key "endColumn"] [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Completion_item_type = struct
  type t =
      Method
    | Function
    | Constructor
    | Field
    | Variable
    | Class
    | Interface
    | Module
    | Property
    | Unit
    | Value
    | Enum
    | Keyword
    | Snippet
    | Text
    | Color
    | File
    | Reference
    | Customcolor
  let of_yojson = function
    | `String "method" -> Ok Method
    | `String "function" -> Ok Function
    | `String "constructor" -> Ok Constructor
    | `String "field" -> Ok Field
    | `String "variable" -> Ok Variable
    | `String "class" -> Ok Class
    | `String "interface" -> Ok Interface
    | `String "module" -> Ok Module
    | `String "property" -> Ok Property
    | `String "unit" -> Ok Unit
    | `String "value" -> Ok Value
    | `String "enum" -> Ok Enum
    | `String "keyword" -> Ok Keyword
    | `String "snippet" -> Ok Snippet
    | `String "text" -> Ok Text
    | `String "color" -> Ok Color
    | `String "file" -> Ok File
    | `String "reference" -> Ok Reference
    | `String "customcolor" -> Ok Customcolor
    | _ -> Error "Debug_protocol.CompletionItemType.t"

  let to_yojson = function
    | Method -> `String "method"
    | Function -> `String "function"
    | Constructor -> `String "constructor"
    | Field -> `String "field"
    | Variable -> `String "variable"
    | Class -> `String "class"
    | Interface -> `String "interface"
    | Module -> `String "module"
    | Property -> `String "property"
    | Unit -> `String "unit"
    | Value -> `String "value"
    | Enum -> `String "enum"
    | Keyword -> `String "keyword"
    | Snippet -> `String "snippet"
    | Text -> `String "text"
    | Color -> `String "color"
    | File -> `String "file"
    | Reference -> `String "reference"
    | Customcolor -> `String "customcolor"
end

module Completion_item = struct
  type t = {
    label : string;
    text : string option [@default None];
    type_ : Completion_item_type.t option [@key "type"] [@default None];
    start : int option [@default None];
    length : int option [@default None];
  } [@@deriving make, yojson { strict = false }]
end

module Value_format = struct
  type t = {
    hex : bool [@default false];
  } [@@deriving make, yojson { strict = false }]
end

module Stack_frame_format = struct
  type t = {
    hex : bool [@default false];
    parameters : bool [@default false];
    parameter_types : bool [@key "parameterTypes"] [@default false];
    parameter_names : bool [@key "parameterNames"] [@default false];
    parameter_values : bool [@key "parameterValues"] [@default false];
    line : bool [@default false];
    module_ : bool [@key "module"] [@default false];
    include_all : bool [@key "includeAll"] [@default false];
  } [@@deriving make, yojson { strict = false }]
end

module Exception_path_segment = struct
  type t = {
    negate : bool [@default false];
    names : string list;
  } [@@deriving make, yojson { strict = false }]
end

module Exception_break_mode = struct
  type t =
      Never
    | Always
    | Unhandled
    | User_unhandled
  let of_yojson = function
    | `String "never" -> Ok Never
    | `String "always" -> Ok Always
    | `String "unhandled" -> Ok Unhandled
    | `String "userUnhandled" -> Ok User_unhandled
    | _ -> Error "Debug_protocol.ExceptionBreakMode.t"

  let to_yojson = function
    | Never -> `String "never"
    | Always -> `String "always"
    | Unhandled -> `String "unhandled"
    | User_unhandled -> `String "userUnhandled"
end

module Exception_options = struct
  type t = {
    path : Exception_path_segment.t list [@default []];
    break_mode : Exception_break_mode.t [@key "breakMode"];
  } [@@deriving make, yojson { strict = false }]
end

module Exception_details = struct
  type t = {
    message : string option [@default None];
    type_name : string option [@key "typeName"] [@default None];
    full_type_name : string option [@key "fullTypeName"] [@default None];
    evaluate_name : string option [@key "evaluateName"] [@default None];
    stack_trace : string option [@key "stackTrace"] [@default None];
    inner_exception : t list [@key "innerException"] [@default []];
  } [@@deriving make, yojson { strict = false }]
end

module Protocol_message = struct
  type t = {
    seq : int;
    type_ : string [@key "type"];
  } [@@deriving make, yojson { strict = false }]
end

module Request = struct
  type t = {
    seq : int;
    type_ : string [@key "type"];
    command : string;
    arguments : Yojson.Safe.json [@default `Assoc []];
  } [@@deriving make, yojson { strict = false }]
end

module Event = struct
  type t = {
    seq : int;
    type_ : string [@key "type"];
    event : string;
    body : Yojson.Safe.json [@default `Assoc []];
  } [@@deriving make, yojson { strict = false }]
end

module Response = struct
  type t = {
    seq : int;
    type_ : string [@key "type"];
    request_seq : int;
    success : bool;
    command : string;
    message : string option [@default None];
    body : Yojson.Safe.json [@default `Assoc []];
  } [@@deriving make, yojson { strict = false }]
end

module ErrorResponse = struct
  module Body = struct
    type t = {
      error : Message.t option [@default None];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Attach_command = struct
  let name = "attach"

  module Request = struct
    module Arguments = struct
      type t = {
        __restart : Yojson.Safe.json [@default `Assoc []];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Completions_command = struct
  let name = "completions"

  module Request = struct
    module Arguments = struct
      type t = {
        frame_id : int option [@key "frameId"] [@default None];
        text : string;
        column : int;
        line : int option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        targets : Completion_item.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Configuration_done_command = struct
  let name = "configurationDone"

  module Request = struct
    module Arguments = struct
      type t = Blank.t [@@deriving yojson]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Continue_command = struct
  let name = "continue"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        all_threads_continued : bool [@key "allThreadsContinued"] [@default false];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Disconnect_command = struct
  let name = "disconnect"

  module Request = struct
    module Arguments = struct
      type t = {
        restart : bool [@default false];
        terminate_debuggee : bool [@key "terminateDebuggee"] [@default false];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Evaluate_command = struct
  let name = "evaluate"

  module Request = struct
    module Arguments = struct
      type t = {
        expression : string;
        frame_id : int option [@key "frameId"] [@default None];
        context : string option [@default None];
        format : Value_format.t option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        result : string;
        type_ : string option [@key "type"] [@default None];
        presentation_hint : Variable_presentation_hint.t option [@key "presentationHint"] [@default None];
        variables_reference : int [@key "variablesReference"];
        named_variables : int option [@key "namedVariables"] [@default None];
        indexed_variables : int option [@key "indexedVariables"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Exception_info_command = struct
  let name = "exceptionInfo"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        exception_id : string [@key "exceptionId"];
        description : string option [@default None];
        break_mode : Exception_break_mode.t [@key "breakMode"];
        details : Exception_details.t option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Goto_command = struct
  let name = "goto"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
        target_id : int [@key "targetId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Goto_targets_command = struct
  let name = "gotoTargets"

  module Request = struct
    module Arguments = struct
      type t = {
        source : Source.t;
        line : int;
        column : int option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        targets : Goto_target.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Initialize_command = struct
  let name = "initialize"

  module Request = struct
    module Arguments = struct
      type t = {
        client_id : string option [@key "clientID"] [@default None];
        client_name : string option [@key "clientName"] [@default None];
        adapter_id : string [@key "adapterID"];
        locale : string option [@default None];
        lines_start_at1 : bool [@key "linesStartAt1"] [@default false];
        columns_start_at1 : bool [@key "columnsStartAt1"] [@default false];
        path_format : string option [@key "pathFormat"] [@default None];
        supports_variable_type : bool [@key "supportsVariableType"] [@default false];
        supports_variable_paging : bool [@key "supportsVariablePaging"] [@default false];
        supports_run_in_terminal_request : bool [@key "supportsRunInTerminalRequest"] [@default false];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Capabilities.t [@@deriving yojson]
    end
  end
end

module Launch_command = struct
  let name = "launch"

  module Request = struct
    module Arguments = struct
      type t = {
        no_debug : bool [@key "noDebug"] [@default false];
        __restart : Yojson.Safe.json [@default `Assoc []];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Loaded_sources_command = struct
  let name = "loadedSources"

  module Request = struct
    module Arguments = struct
      type t = Blank.t [@@deriving yojson]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        sources : Source.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Modules_command = struct
  let name = "modules"

  module Request = struct
    module Arguments = struct
      type t = {
        start_module : int option [@key "startModule"] [@default None];
        module_count : int option [@key "moduleCount"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        modules : Module.t list;
        total_modules : int option [@key "totalModules"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Next_command = struct
  let name = "next"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Pause_command = struct
  let name = "pause"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Restart_command = struct
  let name = "restart"

  module Request = struct
    module Arguments = struct
      type t = Blank.t [@@deriving yojson]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Restart_frame_command = struct
  let name = "restartFrame"

  module Request = struct
    module Arguments = struct
      type t = {
        frame_id : int [@key "frameId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Reverse_continue_command = struct
  let name = "reverseContinue"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Run_in_terminal_command = struct
  let name = "runInTerminal"

  module Request = struct
    module Arguments = struct
      module Kind = struct
        type t =
            Integrated
          | External
        let of_yojson = function
          | `String "integrated" -> Ok Integrated
          | `String "external" -> Ok External
          | _ -> Error "Debug_protocol.Run_in_terminal_command.Request.Arguments.Kind.t"

        let to_yojson = function
          | Integrated -> `String "integrated"
          | External -> `String "external"
      end

      type t = {
        kind : Kind.t option [@default None];
        title : string option [@default None];
        cwd : string;
        args : string list;
        env : String_opt_dict.t [@default String_opt_dict.empty];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        process_id : int option [@key "processId"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Scopes_command = struct
  let name = "scopes"

  module Request = struct
    module Arguments = struct
      type t = {
        frame_id : int [@key "frameId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        scopes : Scope.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Set_breakpoints_command = struct
  let name = "setBreakpoints"

  module Request = struct
    module Arguments = struct
      type t = {
        source : Source.t;
        breakpoints : Source_breakpoint.t list [@default []];
        lines : int list [@default []];
        source_modified : bool [@key "sourceModified"] [@default false];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        breakpoints : Breakpoint.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Set_exception_breakpoints_command = struct
  let name = "setExceptionBreakpoints"

  module Request = struct
    module Arguments = struct
      type t = {
        filters : string list;
        exception_options : Exception_options.t list [@key "exceptionOptions"] [@default []];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Set_expression_command = struct
  let name = "setExpression"

  module Request = struct
    module Arguments = struct
      type t = {
        expression : string;
        value : string;
        frame_id : int option [@key "frameId"] [@default None];
        format : Value_format.t option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        value : string;
        type_ : string option [@key "type"] [@default None];
        presentation_hint : Variable_presentation_hint.t option [@key "presentationHint"] [@default None];
        variables_reference : int option [@key "variablesReference"] [@default None];
        named_variables : int option [@key "namedVariables"] [@default None];
        indexed_variables : int option [@key "indexedVariables"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Set_function_breakpoints_command = struct
  let name = "setFunctionBreakpoints"

  module Request = struct
    module Arguments = struct
      type t = {
        breakpoints : Function_breakpoint.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        breakpoints : Breakpoint.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Set_variable_command = struct
  let name = "setVariable"

  module Request = struct
    module Arguments = struct
      type t = {
        variables_reference : int [@key "variablesReference"];
        name : string;
        value : string;
        format : Value_format.t option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        value : string;
        type_ : string option [@key "type"] [@default None];
        variables_reference : int option [@key "variablesReference"] [@default None];
        named_variables : int option [@key "namedVariables"] [@default None];
        indexed_variables : int option [@key "indexedVariables"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Source_command = struct
  let name = "source"

  module Request = struct
    module Arguments = struct
      type t = {
        source : Source.t option [@default None];
        source_reference : int [@key "sourceReference"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        content : string;
        mime_type : string option [@key "mimeType"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Stack_trace_command = struct
  let name = "stackTrace"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
        start_frame : int option [@key "startFrame"] [@default None];
        levels : int option [@default None];
        format : Stack_frame_format.t option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        stack_frames : Stack_frame.t list [@key "stackFrames"];
        total_frames : int option [@key "totalFrames"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Step_back_command = struct
  let name = "stepBack"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Step_in_command = struct
  let name = "stepIn"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
        target_id : int option [@key "targetId"] [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Step_in_targets_command = struct
  let name = "stepInTargets"

  module Request = struct
    module Arguments = struct
      type t = {
        frame_id : int [@key "frameId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        targets : Step_in_target.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Step_out_command = struct
  let name = "stepOut"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_id : int [@key "threadId"];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Terminate_command = struct
  let name = "terminate"

  module Request = struct
    module Arguments = struct
      type t = {
        restart : bool [@default false];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Terminate_threads_command = struct
  let name = "terminateThreads"

  module Request = struct
    module Arguments = struct
      type t = {
        thread_ids : int list [@key "threadIds"] [@default []];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = Blank.t [@@deriving yojson]
    end
  end
end

module Threads_command = struct
  let name = "threads"

  module Request = struct
    module Arguments = struct
      type t = Blank.t [@@deriving yojson]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        threads : Thread.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Variables_command = struct
  let name = "variables"

  module Request = struct
    module Arguments = struct
      module Filter = struct
        type t =
            Indexed
          | Named
        let of_yojson = function
          | `String "indexed" -> Ok Indexed
          | `String "named" -> Ok Named
          | _ -> Error "Debug_protocol.Variables_command.Request.Arguments.Filter.t"

        let to_yojson = function
          | Indexed -> `String "indexed"
          | Named -> `String "named"
      end

      type t = {
        variables_reference : int [@key "variablesReference"];
        filter : Filter.t option [@default None];
        start : int option [@default None];
        count : int option [@default None];
        format : Value_format.t option [@default None];
      } [@@deriving make, yojson { strict = false }]
    end
  end

  module Response = struct
    module Body = struct
      type t = {
        variables : Variable.t list;
      } [@@deriving make, yojson { strict = false }]
    end
  end
end

module Initialized_event = struct
  let name = "initialized"

  module Body = struct
    type t = Blank.t [@@deriving yojson]
  end
end

module Stopped_event = struct
  let name = "stopped"

  module Body = struct
    type t = {
      reason : string;
      description : string option [@default None];
      thread_id : int option [@key "threadId"] [@default None];
      preserve_focus_hint : bool [@key "preserveFocusHint"] [@default false];
      text : string option [@default None];
      all_threads_stopped : bool [@key "allThreadsStopped"] [@default false];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Continued_event = struct
  let name = "continued"

  module Body = struct
    type t = {
      thread_id : int [@key "threadId"];
      all_threads_continued : bool [@key "allThreadsContinued"] [@default false];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Exited_event = struct
  let name = "exited"

  module Body = struct
    type t = {
      exit_code : int [@key "exitCode"];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Terminated_event = struct
  let name = "terminated"

  module Body = struct
    type t = {
      restart : Yojson.Safe.json [@default `Assoc []];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Thread_event = struct
  let name = "thread"

  module Body = struct
    type t = {
      reason : string;
      thread_id : int [@key "threadId"];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Output_event = struct
  let name = "output"

  module Body = struct
    type t = {
      category : string option [@default None];
      output : string;
      variables_reference : int option [@key "variablesReference"] [@default None];
      source : Source.t option [@default None];
      line : int option [@default None];
      column : int option [@default None];
      data : Yojson.Safe.json [@default `Assoc []];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Breakpoint_event = struct
  let name = "breakpoint"

  module Body = struct
    type t = {
      reason : string;
      breakpoint : Breakpoint.t;
    } [@@deriving make, yojson { strict = false }]
  end
end

module Module_event = struct
  let name = "module"

  module Body = struct
    module Reason = struct
      type t =
          New
        | Changed
        | Removed
      let of_yojson = function
        | `String "new" -> Ok New
        | `String "changed" -> Ok Changed
        | `String "removed" -> Ok Removed
        | _ -> Error "Debug_protocol.Module_event.Body.Reason.t"

      let to_yojson = function
        | New -> `String "new"
        | Changed -> `String "changed"
        | Removed -> `String "removed"
    end

    type t = {
      reason : Reason.t;
      module_ : Module.t [@key "module"];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Loaded_source_event = struct
  let name = "loadedSource"

  module Body = struct
    module Reason = struct
      type t =
          New
        | Changed
        | Removed
      let of_yojson = function
        | `String "new" -> Ok New
        | `String "changed" -> Ok Changed
        | `String "removed" -> Ok Removed
        | _ -> Error "Debug_protocol.Loaded_source_event.Body.Reason.t"

      let to_yojson = function
        | New -> `String "new"
        | Changed -> `String "changed"
        | Removed -> `String "removed"
    end

    type t = {
      reason : Reason.t;
      source : Source.t;
    } [@@deriving make, yojson { strict = false }]
  end
end

module Process_event = struct
  let name = "process"

  module Body = struct
    module Start_method = struct
      type t =
          Launch
        | Attach
        | Attach_for_suspended_launch
      let of_yojson = function
        | `String "launch" -> Ok Launch
        | `String "attach" -> Ok Attach
        | `String "attachForSuspendedLaunch" -> Ok Attach_for_suspended_launch
        | _ -> Error "Debug_protocol.Process_event.Body.Start_method.t"

      let to_yojson = function
        | Launch -> `String "launch"
        | Attach -> `String "attach"
        | Attach_for_suspended_launch -> `String "attachForSuspendedLaunch"
    end

    type t = {
      name : string;
      system_process_id : int option [@key "systemProcessId"] [@default None];
      is_local_process : bool [@key "isLocalProcess"] [@default false];
      start_method : Start_method.t option [@key "startMethod"] [@default None];
    } [@@deriving make, yojson { strict = false }]
  end
end

module Capabilities_event = struct
  let name = "capabilities"

  module Body = struct
    type t = {
      capabilities : Capabilities.t;
    } [@@deriving make, yojson { strict = false }]
  end
end

