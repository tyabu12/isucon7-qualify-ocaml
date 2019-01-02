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