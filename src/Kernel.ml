
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
  ir_cursor_pos: int; (* cursor pos *)
  ir_detail_level: int; (* 0 or 1 *)
}

type inspect_reply_ok = {
  iro_status: string; (* "ok" or "error" *)
  iro_found: bool;
  iro_data: mime_data_bundle;
}

let mime ?(base64=false) ~ty x =
  Mime [{mime_type=ty; mime_content=x; mime_b64=base64}]

let ok ?(actions=[]) msg = {msg; actions}

type t = {
  init: unit -> unit;
  deinit: unit -> unit;
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

let make
    ?banner
    ?(file_extension=".txt")
    ?mime_type
    ?codemirror_mode
    ?(init=fun () -> ())
    ?(deinit=fun () -> ())
    ?(is_complete=fun _ -> Ivar.return Is_complete)
    ?(complete=fun ~pos _i ->
        Ivar.return {completion_matches=[]; completion_start=pos;completion_end=pos})
    ?(inspect=fun _ -> Ivar.fail "no inspection implemented")
    ?(history=fun _ -> Ivar.return [])
    ~language_version
    ~language
    ~exec
    () : t =
  { banner; file_extension; mime_type; language; language_version;
    is_complete; history; exec; complete; inspect;
    init; deinit; codemirror_mode;
  }
