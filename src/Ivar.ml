
open Common_

type ('a, 'err) cb = ('a, 'err) result -> unit

type ('a, 'err) state =
  | Done of ('a, 'err) result
  | Waiting of {mutable cbs: ('a, 'err) cb list}

type ('a, 'err) t = {
  lock: Mutex.t;
  mutable st: ('a, 'err) state;
}

let[@inline] with_lock_ self f =
  Mutex.lock self.lock;
  Fun.protect f ~finally:(fun () -> Mutex.unlock self.lock)

type ('a, 'err) resolver = ('a, 'err) t

let[@inline] is_done self = match self.st with
  | Done _ -> true
  | Waiting _ -> false

let on_done self f =
  let todo =
    let@ () = with_lock_ self in
    match self.st with
    | Done r -> Some r
    | Waiting r ->
      r.cbs <- f :: r.cbs;
      None
  in
  Option.iter (fun r -> f r) todo

let create () =
  let st = {lock=Mutex.create(); st=Waiting {cbs=[]}} in
  st, st

let lock_ = Mutex.create() (* shared for immediate ivars *)

let return x = {lock=lock_; st=Done (Ok x)}
let return_res x = {lock=lock_; st=Done x}
let fail err = {lock=lock_; st=Done (Error err)}

let resolve_ self x =
  let cbs =
    let@ () = with_lock_ self in
    match self.st with
    | Done _ -> invalid_arg "Ivar: already resolved"
    | Waiting {cbs} ->
      self.st <- Done x;
      cbs
  in
  (* execute callbacks outside the lock *)
  List.iter (fun f -> f x) cbs

let[@inline] resolve_ok self x = resolve_ self (Ok x)
let[@inline] resolve_err self x = resolve_ self (Error x)
