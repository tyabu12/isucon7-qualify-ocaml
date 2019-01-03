let array_find_first arr f =
  let rec aux i =
    if i >= Array.length arr then raise Not_found
    else if f arr.(i) then arr.(i)
    else aux (succ i)
  in
  aux 0

(* TODO: output error_log file? *)
let or_die where = function
  | Ok res -> res
  | Error (num, msg) ->
    Format.sprintf "%s (%d) %s" where num msg |> failwith

let env var default =
  try Sys.getenv var with Not_found -> default

let dump_dict dict =
  List.iter begin fun (k, v) ->
    print_string (k ^ ":");
    List.iter (fun s -> print_string s) v;
    print_newline ()
  end dict

let form_value dict key =
  List.assoc key dict |> List.hd

let random_string n =
  let s =
    "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let len = String.length s in
  String.init n (fun _ ->
    Random.int len
    |> String.unsafe_get s
  )

let calc_digest salt password =
  let open Nocrypto in
  let hex_string_of_cstruct c =
    let hexdump_pp fmt =
      for i = 0 to Cstruct.len c - 1 do
        Cstruct.get_char c i |> Char.code |> Format.fprintf fmt "%.2x"
      done;
      Format.pp_print_flush fmt ()
    in
    let b = Buffer.create (Cstruct.len c * 2) in
    let f = Format.formatter_of_buffer b in
    Format.fprintf f "%t" hexdump_pp;
    Buffer.to_bytes b |> Bytes.unsafe_to_string
  in
  salt ^ password
  |> Cstruct.of_string
  |> Hash.SHA1.digest
  |> hex_string_of_cstruct