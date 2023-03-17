open! Core
open! Async

let () =
  Memtrace.trace_if_requested ();
  Command_unix.run Bernard_j_ortcutt.Bot.command
;;
