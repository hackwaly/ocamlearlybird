type rv =
  | Double of float
  | Remote of Debugcom.remote_value

type t = {
  ty : Types.type_expr;
  rv : rv;
}
