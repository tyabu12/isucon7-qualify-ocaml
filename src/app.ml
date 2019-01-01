open Opium.Std

(* TODO: output error_log file? *)
let or_die where = function
  | Ok res -> res
  | Error (num, msg) ->
    Format.sprintf "%s (%d) %s" where num msg |> failwith

module DB = struct
  include Mariadb.Blocking

  (* Just execute query *)
  let just_exec dbh query =
    match prepare dbh query with
    | Ok stmt -> begin
      match Stmt.execute stmt [||] with
      | Ok _ -> Stmt.close stmt
      | Error _ as err -> err
      end
    | Error _ as err -> err
end

let env var default =
  try Sys.getenv var with Not_found -> default

let db =
  let host = env "ISUBATA_DB_HOST" "127.0.0.1" in
  let port = env "ISUBATA_DB_PORT" "3306" |> int_of_string in
  let user = env "ISUBATA_DB_USER" "root" in
  let pass = env "ISUBATA_DB_PASSWORD" "" in
  let dsn =
    let userinfo = user ^ (if pass = "" then "" else ":" ^ pass) in
    Uri.make ~scheme:"tcp" ~host ~port ~userinfo () |> Uri.to_string
  in
  print_endline ("Connecting to " ^ dsn);
  let dbh =
    DB.connect ~host ~port ~user ~pass ~db:"isubata" () |> or_die "DB.connect"
  in
  DB.set_character_set dbh "utf8mb4" |> or_die "DB.set_character_set";
  DB.just_exec dbh {|
      SET SESSION
        sql_mode='TRADITIONAL,NO_AUTO_VALUE_ON_ZERO,ONLY_FULL_GROUP_BY'
    |} |> or_die "DB.just_exec";
  print_endline "Succeeded to connect db.";
  dbh

let random_string n =
  let s = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" in
  let z = String.length s in
  String.init n (fun _ ->
    Random.int z
    |> String.unsafe_get s
  )

let form_value dict key =
  List.assoc key dict |> List.hd

let sess_user_id req =
  let res = Session.get req ~key:"user_id" in
  match res with
  | Some user_id when user_id <> "0" ->
    failwith "Not implemented"
  | _ -> None

let sess_set_user_id res ~user_id:user_id =
  Session.set res ~key:"user_id" ~data:user_id ()

let register user_name passwd =
  let open Nocrypto in
  let salt = random_string 20 in
  let digest =
    salt ^ passwd
    |> Cstruct.of_string
    |> Hash.SHA1.digest
  in
  let digest =
    let b = Buffer.create 30 in
    Cstruct.hexdump_to_buffer b digest;
    Buffer.to_bytes b |> Bytes.unsafe_to_string
  in
  print_endline begin
    "user_name:" ^ user_name ^ " ,"
    ^ "salt:" ^ salt ^ " ,"
    ^ "digest:" ^ digest
  end;
  failwith "Not implemented"

let no_content = `String ""

let redirect_to_login () = "/login" |> Uri.of_string |> redirect'

let get_mock req = begin
  ignore req;
  failwith "Not implemented"
end

let post_mock dict = begin
  ignore dict;
  failwith "Not implemented"
end

(* routes *)

let get_initialize = get "/initialize" begin fun _ ->
  let db_just_exec query = DB.just_exec db query |> or_die "get_initialize" in
  db_just_exec "DELETE FROM user WHERE id > 1000";
  db_just_exec "DELETE FROM image WHERE id > 1001";
  db_just_exec "DELETE FROM channel WHERE id > 10";
  db_just_exec "DELETE FROM message WHERE id > 10000";
  db_just_exec "DELETE FROM haveread";
  no_content |> respond' ~code:`No_content
end

let get_index = get "/" begin fun req ->
  match sess_user_id req with
  | None -> `Html Views.Index.html |> respond'
  | Some _ -> "/channel/1" |> Uri.of_string |> redirect'
end

let get_channel = get "/channel/:channel_id" begin fun req ->
  let user_id = sess_user_id req in
  let channel_id = "channel_id" |> param req in
  match user_id, channel_id with
  | None, _ -> redirect_to_login ()
  | Some _, channel_id ->
    let desc = "TODO: チャンネルID(" ^ channel_id ^ ") の説明" in
    `Html (Views.Channel.html false desc) |> respond'
end

let get_register = get "/register" begin fun _ ->
  `Html Views.Register.html |> respond'
end

let post_register = post "/register" begin fun req ->
  App.urlencoded_pairs_of_body req
  |> Lwt.map begin fun dict ->
    let name = List.hd (List.assoc "name" dict) in
    let passwd = List.hd (List.assoc "password" dict) in
    if String.length name = 0 || String.length passwd = 0 then
      no_content |> respond ~code:`Bad_request
    else begin
      let user_id = register name passwd  in
      redirect (Uri.of_string "/")
      |> sess_set_user_id ~user_id
    end
  end
end

let get_login = get "/login" begin fun _ ->
  `Html Views.Login.html |> respond'
end

let post_login = post "/login" begin fun req ->
  req
  |> App.urlencoded_pairs_of_body
  |> Lwt.map begin fun dict ->
    let name = List.assoc "name" dict |> List.hd in
    let passwd = List.assoc "password" dict |> List.hd in
    print_endline begin
      "name:" ^ name  ^ ", "
      ^ "password_length;" ^ (String.length passwd |> string_of_int)
    end;
    failwith "Not implemented"
  end
end

let get_logout = get "/logout" begin fun _ ->
  redirect (Uri.of_string "/")
  |> Session.delete ~key:"user_id"
  |> Lwt.return
end

let post_message = post "/message" begin fun req ->
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    let user_id =
      match sess_user_id req with
      | Some user_id -> user_id
      | None -> failwith "post_message" (* FIXME? *)
    in
    match form_value dict "message", form_value dict "channel_id" with
    | exception _ ->
      no_content |> respond ~code:`Forbidden
    | message, channel_id when message = "" || channel_id = "" ->
      no_content |> respond ~code:`Forbidden
    | message, channel_id ->
      print_endline begin
        "user_id:" ^ user_id  ^ ", "
        ^ "message:" ^ message ^ ", "
        ^ "channel_id:" ^ channel_id
      end;
      failwith "Not implemented"
  end
end

let get_message = get "/message" get_mock

let fetch_unread = get "/fetch" get_mock

let get_history = get "/history" get_mock

let get_profile = get "/profile" get_mock

let get_add_channel = get "/add_channel" get_mock

let post_add_channel = post "/add_channel" begin fun req ->
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    post_mock dict
  end
end

let post_profile = post "/profile" begin fun req ->
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    post_mock dict
  end
end

let get_icon = get "/icon/:filename" get_mock

let () =
  Random.self_init ();
  Nocrypto_entropy_unix.initialize ();
  let public_dir = env "ISUBATA_PUBLIC_DIR" "../public" in
  let port = env "ISUBATA_APP_PORT" "3000" |> int_of_string in
  print_endline ("expose port: " ^ (port |> string_of_int));
  let app =
    App.empty
    |> App.cmd_name "ISUCON7-qualify-ocaml"
    |> App.port port
    |> middleware Cookie.m
    |> middleware Middleware.trace
    |> middleware (Middleware.static ~local_path:public_dir ~uri_prefix:"/" ())
    |> get_initialize
    |> get_index
    |> get_register
    |> post_register
    |> get_login
    |> post_login
    |> get_logout
    |> get_channel
    |> get_message
    |> post_message
    |> fetch_unread
    |> get_history
    |> get_profile
    |> post_profile
    |> get_add_channel
    |> post_add_channel
    |> get_icon
    |> App.run_command' in
  match app with
  | `Ok app ->
    Lwt_main.at_exit (fun () -> DB.library_end () |> Lwt.return);
    Lwt_main.run app
  | `Error -> DB.library_end (); exit 1
  | `Not_running -> exit 0
