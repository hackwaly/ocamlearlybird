let src = Logs.Src.create "ocaml_debug_agent"

include (val Logs_lwt.src_log src : Logs_lwt.LOG)
