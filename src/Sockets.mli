(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

(** {1 Sockets to Jupyter}

    See https://jupyter-client.readthedocs.io/en/latest/messaging.html *)

type t = private {
  shell : [`Router] Zmq.Socket.t;
  control : [`Router] Zmq.Socket.t;
  stdin : [`Router] Zmq.Socket.t;
  iopub : [`Pub] Zmq.Socket.t;
  heartbeat: [`Rep ] Zmq.Socket.t;
  mutable exit: bool;
}

val open_sockets : Protocol_t.connection_info -> t

val close : t -> unit

val heartbeat_loop : t -> unit
