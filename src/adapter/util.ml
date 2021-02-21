open Debug_protocol_ex

let _get_shift x =
  if x |> Option.value ~default:false then 0 else 1

let line_from_client ~(init_args : Initialize_command.Arguments.t) line =
  line + _get_shift init_args.lines_start_at1

let column_from_client ~(init_args : Initialize_command.Arguments.t) column =
  column + _get_shift init_args.columns_start_at1

let line_to_client ~(init_args : Initialize_command.Arguments.t) line =
  line - _get_shift init_args.lines_start_at1

let column_to_client ~(init_args : Initialize_command.Arguments.t) column =
  column - _get_shift init_args.columns_start_at1
