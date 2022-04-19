(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

open Common_
open Protocol_j
open Kernel

module M = Message

(** {2 Prelude} *)

type 'a or_error = ('a, string) result
type 'a ivar = ('a, string) Ivar.t

type run_result =
  | Run_stop
  | Run_restart
  | Run_fail of exn

exception E_res of run_result

let trigger_restart () = raise (E_res (Run_restart))

type t = {
  sockets: Sockets.t;
  key: string option;
  kernel: Kernel.t;
  incoming: M.t SQ.t;
  mutable e_count: int;
  mutable exit: bool;
  mutable needs_interrupt: bool; (* TODO: atomic *)
  mutable interrupted: (unit, unit) Ivar.t;
  mutable interrupt: (unit, unit) Ivar.resolver;
}

let make ?key sockets kernel : t =
  let interrupted, interrupt = Ivar.create() in
  { key; sockets; kernel; e_count=0;
    needs_interrupt=false; exit=false;
    interrupted; interrupt;
    incoming=SQ.create();
  }

type iopub_message =
  | Iopub_send_message of Message.content
  | Iopub_send_mime of mime_data_bundle

let string_of_message content =
  let msg_type = M.msg_type_of_content content in
  Printf.sprintf "{`%s` content `%s`}"
    msg_type (M.json_of_content content)

let string_of_mime_data (m:mime_data): string =
  Printf.sprintf "mime{ty=%s; content=%S; base64=%B}"
    m.mime_type m.mime_content m.mime_b64

let string_of_mime_data_bundle l : string =
  "[" ^ String.concat "\n" (List.map string_of_mime_data l) ^ "]"

let string_of_iopub_message = function
  | Iopub_send_message content ->
    Printf.sprintf "send_message %s" (string_of_message content)
  | Iopub_send_mime l ->
    Printf.sprintf "send_mime %s" (string_of_mime_data_bundle l)

let dict_of_mime_bundle (l:mime_data_bundle): json =
  let l =
    l
    |> List.map (fun m ->
        let data =
          if not m.mime_b64 then m.mime_content
          else Base64.encode m.mime_content
        in
        m.mime_type, `String data)
  in
  `Assoc l

(* encode mime data, wrap it into a message *)
let mime_message_content (m:mime_data_bundle) : M.content =
  Message.Display_data (Protocol_j.({
      dd_data = dict_of_mime_bundle m;
      dd_metadata = `Assoc [];
      dd_transient=None; (* TODO *)
    }))

let send_shell (self:t) ~parent (content:M.content): unit =
  Log.debug (fun k->k "send_shell `%s`" (string_of_message content));
  let socket = self.sockets.Sockets.shell in
  let msg_type = M.msg_type_of_content content in
  let msg' = M.make ~parent ~msg_type content in
  M.send ?key:self.key socket msg'

(* send a message on the Iopub socket *)
let send_iopub (self:t) ?parent (m:iopub_message): unit =
  Log.debug (fun k->k "send_iopub `%s`" (string_of_iopub_message m));
  let socket = self.sockets.Sockets.iopub in
  let send_message msg_type content =
    let msg' = match parent with
      | None -> M.make_first ~msg_type content
      | Some parent -> M.make ~parent ~msg_type content
    in
    M.send ?key:self.key socket msg'
  in
  let send_mime (l:mime_data_bundle) =
    (* send mime message *)
    let content = mime_message_content l in
    let msg_type = M.msg_type_of_content content in
    send_message msg_type content
  in
  begin match m with
    | Iopub_send_message content ->
      let msg_type = M.msg_type_of_content content in
      send_message msg_type content
    | Iopub_send_mime l ->
      send_mime l
  end

(* run [f ()] in a "status:busy" context *)
let within_status ~parent (t:t) ~f =
  (* set state to busy *)
  send_iopub ~parent t
    (Iopub_send_message (M.Status { execution_state = "busy" }));
  Fun.protect f
    ~finally:(fun () ->
        send_iopub ~parent t
          (Iopub_send_message
             (M.Status { execution_state = "idle" })))

(* execute code *)
let execute_request (self:t) ~parent e : unit =
  (* if we are not silent increment execution count *)
  if not e.silent then (
    self.e_count <- self.e_count + 1;
  );

  let execution_count = self.e_count in

  send_iopub self ~parent
    (Iopub_send_message
       (M.Execute_input {
           pi_code = e.code;
           pi_execution_count = execution_count;
         }));

  (* in case of success, how to print *)
  let reply_status_ok (s:string option) = match s with
    | None -> ()
    | Some msg ->
      send_iopub self ~parent (Iopub_send_message
                              (M.Execute_result {
                                  po_execution_count = execution_count;
                                  po_data = `Assoc ["text/plain", `String msg];
                                  po_metadata = `Assoc []; }))
  and side_action (s:Kernel.exec_action) : unit = match s with
    | Kernel.Mime l -> send_iopub self ~parent (Iopub_send_mime l)
  in

  (* eval code *)
  let status = self.kernel.Kernel.exec ~count:execution_count e.code in

  Ivar.on_done status @@ function
  | Ok ok ->
    send_shell self ~parent
      (M.Execute_reply {
          status = "ok";
          execution_count;
          ename = None; evalue = None; traceback = None; payload = None;
          er_user_expressions = None;
        });
    reply_status_ok ok.Kernel.msg;
    (* send mime type in the background *)
    List.iter side_action ok.Kernel.actions;
  | Error err_msg ->
    let content =
      M.Execute_reply {
        status = "error";
        execution_count;
        ename = Some "error"; evalue = Some err_msg;
        traceback = Some ["<eval>"]; payload = None;
        er_user_expressions = None;
      }
    in
    Log.debug (fun k->k "send ERROR `%s`" (M.json_of_content content));
    send_shell self ~parent content;
    send_iopub self ~parent
      (Iopub_send_message (M.Execute_error {
           err_ename="error";
           err_evalue=err_msg;
           err_traceback=[
             "ERROR " ^ err_msg;
             "evaluating " ^ e.code;
           ];
         }))

let kernel_info_request (self:t) ~parent =
  let str_of_version l = String.concat "." (List.map string_of_int l) in
  begin
    send_shell self ~parent (M.Kernel_info_reply {
        implementation = self.kernel.Kernel.language;
        implementation_version =
          str_of_version self.kernel.Kernel.language_version;
        protocol_version = "5.0";
        language_info = {
          li_name = self.kernel.Kernel.language;
          li_version = str_of_version self.kernel.Kernel.language_version;
          li_mimetype=(match self.kernel.Kernel.mime_type with
              | Some m -> m
              | None -> "text"
            );
          li_file_extension=self.kernel.Kernel.file_extension;
          li_codemirror_mode=self.kernel.Kernel.codemirror_mode;
        };
        banner= (match self.kernel.Kernel.banner with
            | None -> ""
            | Some b -> b);
        help_links=[];
      })
  end

let comm_info_request (t:t) ~parent =
  send_shell t ~parent M.Comm_info_reply

let shutdown_request (self:t) ~parent (r:shutdown) : 'a =
  Log.info (fun k->k "received shutdown request...");
  begin
    try
      send_shell self ~parent (M.Shutdown_reply r);
    with e ->
       Log.err (fun k->k "exn %s when replying to shutdown request" (Printexc.to_string e));
  end;
  raise (E_res (if r.restart then Run_restart else Run_stop))

let interrupt_request (self:t) ~parent : unit =
  Log.info (fun k->k "received interrupt request...");
  send_shell self ~parent (M.Interrupt_reply {status="ok"})

let handle_invalid_message () =
  failwith "Invalid message on shell socket"

(* translate positions in codepoints, to positions in bytes offset *)
let byte_pos_of_utf_pos s ~cursor_pos : int =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec iter n =
    if n=0 then Uutf.decoder_byte_count dec (* done *)
    else match Uutf.decode dec with
      | `Await -> assert false
      | `End -> String.length s + 1 (* out of range *)
      | `Malformed _ ->
        (* skip malformed substring *)
        iter (n-1)
      | `Uchar _ ->
        iter (n-1)
  in
  assert (cursor_pos >= 0);
  iter cursor_pos

(* translate positions in byte offsets, to positions in codepoints *)
let utf_pos_of_byte_pos s ~pos : int =
  let dec = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let rec iter n =
    if Uutf.decoder_byte_count dec >= pos
    then Uutf.decoder_count dec (* done *)
    else match Uutf.decode dec with
      | `Await -> assert false
      | `End -> Uutf.decoder_count dec + 1 (* out of range *)
      | `Malformed _ ->
        (* skip malformed substring *)
        iter (n-1)
      | `Uchar _ ->
        iter (n-1)
  in
  assert (pos >= 0);
  iter pos

let complete_request (self:t) ~parent (r:complete_request): unit =
  let pos = byte_pos_of_utf_pos ~cursor_pos:r.cursor_pos r.line in
  let res = self.kernel.Kernel.complete ~pos r.line in
  Ivar.on_done res @@ function
  | Ok st ->
    let content = {
      matches=st.Kernel.completion_matches;
      cursor_start=utf_pos_of_byte_pos r.line ~pos:st.Kernel.completion_start;
      cursor_end=utf_pos_of_byte_pos r.line ~pos:st.Kernel.completion_end;
      cr_status="ok";
    } in
    send_shell self ~parent (M.Complete_reply content)
  | Error err ->
    Log.err (fun k->k"complete request failed: %s" err)

let is_complete_request (self:t) ~parent (r:is_complete_request): unit =
  let res = self.kernel.Kernel.is_complete r.icr_code in
  Ivar.on_done res @@ function
  | Ok st ->
    let content = match st with
      | Kernel.Is_complete ->
        {icr_status="complete"; icr_indent=""}
      | Kernel.Is_not_complete icr_indent ->
        {icr_status="incomplete"; icr_indent}
    in
    send_shell self ~parent (M.Is_complete_reply content)
  | Error err ->
    Log.err (fun k->k"is complete request failed: %s" err)

let inspect_request (self:t) ~parent (r:Kernel.inspect_request) =
  let res =
    let pos = byte_pos_of_utf_pos ~cursor_pos:r.ir_cursor_pos r.ir_code in
    self.kernel.Kernel.inspect {r with ir_cursor_pos=pos}
  in
  Ivar.on_done res @@ fun res ->
  let content = match res with
    | Ok r ->
      {
        ir_status = "ok";
        ir_found = Some r.Kernel.iro_found;
        ir_data = Some (dict_of_mime_bundle r.Kernel.iro_data);
        ir_metadata=None; (* TODO *)
        ir_ename =None; ir_evalue=None; ir_traceback=None;
      }
    | Error err_msg ->
      {
        ir_status = "error";
        ir_found=None; ir_data=None; ir_metadata=None;
        ir_ename = Some "error";
        ir_evalue = Some err_msg;
        ir_traceback = Some ["<inspect_request>"];
      }
  in
  send_shell self ~parent (M.Inspect_reply content)

let connect_request _socket _msg = () (* XXX deprecated *)

let history_request t ~parent x =
  let res = t.kernel.Kernel.history x in
  Ivar.on_done res @@ function
  | Ok history ->
    let content = {history} in
    send_shell t ~parent (M.History_reply content)
  | Error err ->
    Log.err (fun k->k"history request failed: %s" err)

let setup_signal_handlers (self:t) =
  (* wake future, but not now, we're in the signal handler. *)
  let on_sig _ =
    self.needs_interrupt <- true;
  in
  let handlesig s =
    Sys.set_signal s (Sys.Signal_handle on_sig);
  in
  handlesig Sys.sigint;
  handlesig Sys.sigterm

(* trigger interruption *)
let handle_interrupt self =
  (* allocate new ivar for interruptions *)
  let oldresolve = self.interrupt in
  let newivar, resolve = Ivar.create () in
  self.interrupted <- newivar;
  self.interrupt <- resolve;
  Ivar.resolve_ok oldresolve ()

let interrupt_thread self () =
  while not self.exit do
    if self.needs_interrupt then (
      self.needs_interrupt <- false;
      handle_interrupt self;
    );
    Thread.delay 0.2;
  done

let forward_msg self sock : unit =
  while not self.exit do
    let m = M.recv sock in
    SQ.push self.incoming m
  done

let run (self:t) : run_result =

  setup_signal_handlers self;
  spawn interrupt_thread self;

  Log.debug (fun k->k "run on sockets...");
  spawn Sockets.heartbeat_loop self.sockets;
  send_iopub self
    (Iopub_send_message (M.Status { execution_state = "starting" }));

  (* initialize *)
  Log.debug (fun k->k "call kernel.init");
  self.kernel.Kernel.init ();

  (* forward messages from these sockets to [self.incoming] *)
  spawn (forward_msg self) self.sockets.shell;
  spawn (forward_msg self) self.sockets.control;

  let handle_message m : unit =
    Log.debug (fun k->k "received message `%s`, content `%s`"
                  (M.msg_type_of_content m.M.content)
                  (M.json_of_content m.M.content));
    begin match m.M.content with
      | M.Kernel_info_request ->
        within_status ~parent:m self
          ~f:(fun () -> kernel_info_request self ~parent:m)
      | M.Comm_info_request _r -> comm_info_request self ~parent:m
      | M.Execute_request x ->
        within_status ~parent:m self
          ~f:(fun () -> execute_request self ~parent:m x)
      | M.Connect_request ->
        Log.warn (fun k->k "warning: received deprecated connect_request");
        connect_request self m;
      | M.Inspect_request x ->
        within_status ~parent:m self ~f:(fun () -> inspect_request self ~parent:m x)
      | M.Complete_request x ->
        within_status ~parent:m self ~f:(fun () -> complete_request self ~parent:m x)
      | M.Is_complete_request x ->
        within_status ~parent:m self ~f:(fun () -> is_complete_request self ~parent:m x)
      | M.History_request x ->
        within_status ~parent:m self ~f:(fun () -> history_request self ~parent:m x)
      | M.Shutdown_request x -> shutdown_request self ~parent:m x
      | M.Interrupt_request -> interrupt_request self ~parent:m

      (* messages we should not be getting *)
      | M.Connect_reply _ | M.Kernel_info_reply _
      | M.Shutdown_reply _ | M.Execute_reply _ | M.Interrupt_reply _
      | M.Inspect_reply _ | M.Complete_reply _ | M.Is_complete_reply _
      | M.History_reply _ | M.Status _ | M.Execute_input _
      | M.Execute_result _ | M.Stream _ | M.Display_data _
      | M.Execute_error _ | M.Clear _ | M.Comm_info_reply ->
        handle_invalid_message ()

      | M.Comm_open -> ()
    end
  in

  let main_loop () =
    while not self.exit do
      let m = SQ.take_block self.incoming in
      try handle_message m
      with
      | Sys.Break ->
        Log.debug (fun k->k "Sys.Break");
        handle_interrupt self
    done;
    Run_stop
  in

  let res =
    try
      main_loop ()
    with
    | E_res res ->
      begin match res with
        | Run_restart ->
          Log.info (fun k->k "Restart");
        | Run_stop ->
          Log.info (fun k->k "Exiting, as requested");
        | Run_fail _ -> ()
      end;
      res
    | e ->
      Log.err (fun k->k "uncaught exception %s" @@ Printexc.to_string e);
      Run_fail e
  in
  Log.debug (fun k->k "call kernel.deinit");
  self.kernel.Kernel.deinit ();
  res

