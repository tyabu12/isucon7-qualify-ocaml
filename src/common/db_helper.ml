open Misc
module DB = Mariadb.Blocking

(* val exec : t -> string -> ?params:Field.value array -> unit ->
    (Stmt.t * Res.t, Mariadb.Blocking.error) result *)
let exec dbh query ?(params = [||]) () =
  match DB.prepare dbh query with
  | Ok stmt -> begin
      match DB.Stmt.execute stmt params with
      | Ok res -> Ok (stmt, res)
      | Error _ as err -> DB.Stmt.close stmt |> ignore; err
    end
  | Error _ as err -> err

(* Just execute query. return nothing *)
(* val just_exec : t -> string -> ?params:Field.value array -> unit
    -> unit Mariadb.Blocking.result *)
let just_exec dbh query ?(params = [||]) () =
  match exec dbh query ~params () with
  | Ok (stmt, _) -> DB.Stmt.close stmt
  | Error _ as err -> err

let last_insert_id dbh =
  let query = "SELECT LAST_INSERT_ID()" in
  let stmt, res = exec dbh query () |> or_die "last_insert_id" in
  assert (DB.Res.num_rows res = 1);
  let id =
    match DB.Res.fetch (module DB.Row.Array) res |> or_die "last_insert_id" with
    | Some row -> row.(0) |> DB.Field.int
    | None -> assert false
  in
  DB.Stmt.close stmt |> or_die "last_insert_id";
  id

let row_find row key value_of_field =
  match DB.Row.StringMap.find_opt key row with
  | Some field -> Some (value_of_field field)
  | None -> None

(* hack: text type column cannot be applyed DB.Field.string *)
let string_of_text_field field =
  DB.Field.bytes field |> Bytes.to_string