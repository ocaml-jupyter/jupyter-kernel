
type json = Yojson.Safe.t

let (let@) f x = f x

let spawn f x = ignore (Thread.create f x : Thread.t)
