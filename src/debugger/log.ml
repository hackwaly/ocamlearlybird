let src = Logs.Src.create "debugger"

include (val Logs_lwt.src_log src : Logs_lwt.LOG)
