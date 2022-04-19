
(** User-defined Kernel. *)

type 'a ivar = ('a, string) Ivar.t

type mime_data = {
  mime_type: string;
  mime_content: string; (* raw content *)
  mime_b64: bool; (* if true, content will be encoded with base64 *)
}

(* list of mime objects, all of distinct types *)
type mime_data_bundle = mime_data list

type exec_action =
  | Mime of mime_data_bundle

(* TODO:
   make the [exec] return type be asynchronous
   - return [Ok str] as a future
   - return a stream of actions
*)

type exec_status_ok = {
  msg: string option;
  (* main message *)
  actions: exec_action list;
  (* other actions *)
}

type completion_status = {
  completion_matches: string list;
  completion_start: int;
  completion_end: int;
}

type is_complete_reply =
  | Is_complete
  | Is_not_complete of string (* indent *)

type history_request = Protocol_j.history_request

type inspect_request = Protocol_j.inspect_request = {
  ir_code: string;
  ir_cursor_pos: int; (* cursor pos, as offset in bytes *)
  ir_detail_level: int; (* 0 or 1 *)
}

type inspect_reply_ok = {
  iro_status: string; (* "ok" or "error" *)
  iro_found: bool;
  iro_data: mime_data_bundle;
}

val mime : ?base64:bool -> ty:string -> string -> exec_action

val ok : ?actions:exec_action list -> string option -> exec_status_ok

type t = private {
  init: unit -> unit;
  deinit: unit -> unit;
  (** Called before stopping the kernel.
      @since 0.8 *)

  exec: count:int -> string -> exec_status_ok ivar; (* TODO: user expressions *)
  is_complete: string -> is_complete_reply ivar;
  language: string;
  language_version: int list;
  banner: string option; (* displayed at startup *)
  file_extension: string;
  mime_type: string option; (* default: text/plain *)
  codemirror_mode: string option; (* client side syntax highlighting mode *)
  complete: pos:int -> string -> completion_status ivar; (* [pos]: offset in bytes in string *)
  inspect: inspect_request -> inspect_reply_ok ivar;
  history: history_request -> string list ivar;
}
(** Type of Jupyter kernels.

    A kernel is responsible for running snippets of code from cells in
    a notebook, or command line statements. It can also answer
    semantic queries such as completion, inspection, etc.

    The type is a private alias as off 0.8. Use {!make} to build.
*)

val make :
  ?banner:string ->
  ?file_extension:string ->
  ?mime_type:string ->
  ?codemirror_mode:string ->
  ?init:(unit -> unit) ->
  ?deinit:(unit -> unit) ->
  ?is_complete:(string -> is_complete_reply ivar) ->
  ?complete:(pos:int -> string -> completion_status ivar) ->
  ?inspect: (inspect_request -> inspect_reply_ok ivar) ->
  ?history:(history_request -> string list ivar) ->
  language_version:int list ->
  language:string ->
  exec:(count:int -> string -> exec_status_ok ivar) ->
  unit ->
  t
(** Make a kernel *)
