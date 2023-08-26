open! Core
open! Async

let () =
  Memtrace.trace_if_requested ();
  Log.set_output Reddit_api_async.Logging.log (Log.get_output (force Log.Global.log));
  Command_unix.run Bernard_j_ortcutt.Bot.command
;;
