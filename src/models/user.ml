open Misc
module DB = Mariadb.Blocking

type t = {
  id : int option;
  name : string option;
  salt : string option;
  password : string option;
  display_name : string option;
  avatar_icon : string option;
  created_at : DB.Time.t option;
}

let get_from_row row =
  let find key value_of_field = Db_helper.row_find row key value_of_field in
  let id = find "id" DB.Field.int in
  let name = find "name" DB.Field.string in
  let salt = find "salt" DB.Field.string in
  let password = find "password" DB.Field.string in
  let display_name = find "display_name" Db_helper.string_of_text_field in
  let avatar_icon = find "avatar_icon" Db_helper.string_of_text_field in
  let created_at = find "created_at" DB.Field.time in
  {id; name; salt; password; display_name; avatar_icon; created_at}

let get db user_id =
  let or_die res = or_die "User.get" res in
  let stmt = DB.prepare db "SELECT * FROM user WHERE id = ?" |> or_die in
  let res = DB.Stmt.execute stmt [| `Int user_id |] |> or_die in
  assert (DB.Res.num_rows res = 1);
  let user_opt =
    match DB.Res.fetch (module DB.Row.Map) res |> or_die with
    | Some row -> Some (get_from_row row)
    | None -> None
  in
  DB.Stmt.close stmt |> ignore;
  user_opt