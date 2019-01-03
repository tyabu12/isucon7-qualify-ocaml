open Misc
module DB = Db_helper.DB

type t = {
  id : int option;
  name : string option;
  description : string option;
  updated_at: DB.Time.t option;
  created_at : DB.Time.t option;
}

let get_from_row row =
  let find key value_of_field = Db_helper.row_find row key value_of_field in
  let id = find "id" DB.Field.int in
  let name = find "name" Db_helper.string_of_text_field in
  let description = find "description" Db_helper.string_of_text_field in
  let updated_at = find "updated_at" DB.Field.time in
  let created_at = find "created_at" DB.Field.time in
  {id; name; description; updated_at; created_at}

let all db =
  let or_die res = or_die "Channel.get" res in
  let stmt, res =
    Db_helper.exec db "SELECT * FROM channel ORDER BY id" () |> or_die in
  let channels = Array.init (DB.Res.num_rows res) begin fun _ ->
    match DB.Res.fetch (module DB.Row.Map) res |> or_die with
    | Some row -> get_from_row row
    | None -> assert false
  end
  in
  DB.Stmt.close stmt |> or_die;
  channels

let insert db name desc =
  print_endline (
    "Insert channel\n" ^
    "name:" ^ name ^ ", " ^
    "description:" ^ desc);
  let query = {|
    INSERT INTO channel
      (name, description, updated_at, created_at)
    VALUES
      (?, ?, NOW(), NOW())
  |} in
  let params =
    let name = `String name in
    let desc = `String desc in
    [| name; desc |]
  in
  Db_helper.just_exec db query ~params () |> or_die "Channel.insert";
  print_endline "Succeeded to inserting channel.";
  Db_helper.last_insert_id db