open Common_

type 'a t = {
  q : 'a Queue.t;
  lock : Mutex.t;
  cond : Condition.t;
}

let create () =
  {
    q=Queue.create();
    lock=Mutex.create();
    cond=Condition.create();
  }

let[@inline] with_lock_ self f =
  Mutex.lock self.lock;
  Fun.protect f ~finally:(fun () -> Mutex.unlock self.lock)

let push self x =
  let@ () = with_lock_ self in
  Queue.push x self.q;
  Condition.broadcast self.cond

let take_block self =
  let@ () = with_lock_ self in
  while Queue.is_empty self.q do
    Condition.wait self.cond self.lock;
  done;
  Queue.take self.q

let try_take self =
  let@ () = with_lock_ self in
  Queue.take_opt self.q

let peek self =
  let@ () = with_lock_ self in
  try Some (Queue.peek self.q)
  with Queue.Empty -> None
