(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

let context = Zmq.Context.create ()
let () = at_exit
    (fun () -> Zmq.Context.terminate context)

let addr conn port =
  Protocol_j.(conn.transport ^ "://" ^ conn.ip ^ ":" ^ string_of_int port)

let open_socket typ conn port =
  let socket = Zmq.Socket.create context typ in
  let addr = addr conn port in
  let () = Zmq.Socket.bind socket addr in
  Log.debug (fun k->k "open and bind socket %s" addr);
  socket

let close_socket s = Zmq.Socket.close s

type t = {
  shell : [`Router] Zmq.Socket.t;
  control : [`Router] Zmq.Socket.t;
  stdin : [`Router] Zmq.Socket.t;
  iopub : [`Pub] Zmq.Socket.t;
  heartbeat: [`Rep ] Zmq.Socket.t;
  mutable exit: bool; (* TODO: use atomic *)
}

let open_sockets conn =
  let open Protocol_j in
  Log.debug (fun k->k "open sockets `%s`" (string_of_connection_info conn));
  { shell = open_socket Zmq.Socket.router conn conn.shell_port;
    control = open_socket Zmq.Socket.router conn conn.control_port;
    stdin = open_socket Zmq.Socket.router conn conn.stdin_port;
    iopub = open_socket Zmq.Socket.pub conn conn.iopub_port;
    heartbeat = open_socket Zmq.Socket.rep conn conn.hb_port;
    exit = false;
  }

let close (self:t) : unit =
  if not self.exit then (
    self.exit <- true;
    close_socket self.shell;
    close_socket self.control;
    close_socket self.stdin;
    close_socket self.iopub;
    close_socket self.heartbeat;
  )

let heartbeat_loop (self:t) =
  Log.debug (fun k->k "listening for hearbeat requests");
  while not self.exit do
    let data = Message.wrap_retry Zmq.Socket.recv self.heartbeat in
    Log.debug (fun k->k "Heartbeat received");
    Message.wrap_retry (Zmq.Socket.send self.heartbeat) data;
  done;
  (* XXX close down properly...we may never get here *)
  Zmq.Socket.close self.heartbeat
