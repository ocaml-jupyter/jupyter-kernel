
type json = Yojson.Safe.t

let (let@) f x = f x

(** Spawn a new thread *)
let spawn_thread f x =
  let run() =
    List.iter (fun i -> Sys.set_signal i Sys.Signal_ignore)
      [Sys.sigint; Sys.sigpipe; Sys.sigterm];
    f x
  in
  ignore (Thread.create run () : Thread.t)
