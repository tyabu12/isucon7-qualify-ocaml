open Opium.Std
open Misc
module DB = Db_helper.DB
module M = Models

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
  Db_helper.just_exec dbh {|
      SET SESSION
        sql_mode='TRADITIONAL,NO_AUTO_VALUE_ON_ZERO,ONLY_FULL_GROUP_BY'
    |} () |> or_die "DB.just_exec";
  print_endline "Succeeded to connect db.";
  dbh

let sess_user_id req =
  match  Session.get req ~key:"user_id" with
  | Some user_id when user_id <> "0" -> Some (int_of_string user_id)
  | _ -> None

let sess_set_user_id res ~user_id =
  let user_id = string_of_int user_id in
  Session.set res ~key:"user_id" ~data:user_id ()

let ensure_login req =
  match sess_user_id req with
  | None -> failwith "ensure_login" (* FIXME? *)
  | Some user_id ->
    match M.User.find db user_id with
    | Some user -> user
    | None ->
      (* TODO: Session.delete ~key:"user_id" *)
      failwith "ensure_login"

let no_content = `String ""

(* overwrite opium *)
let redirect ?headers ?(code=`Found) uri =
  let headers = Cohttp.Header.add_opt headers "Location" (Uri.to_string uri) in
  Response.create ~headers ~code ()

(* overwrite opium *)
let redirect' ?headers ?(code=`Found) uri =
  uri |> redirect ?headers ~code |> Lwt.return

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
  print_endline "GET /initialize";
  let db_just_exec query =
    Db_helper.just_exec db query () |> or_die "get_initialize"
  in
  db_just_exec "DELETE FROM user WHERE id > 1000";
  db_just_exec "DELETE FROM image WHERE id > 1001";
  db_just_exec "DELETE FROM channel WHERE id > 10";
  db_just_exec "DELETE FROM message WHERE id > 10000";
  db_just_exec "DELETE FROM haveread";
  no_content |> respond' ~code:`No_content
end

let get_index = get "/" begin fun req ->
  print_endline "GET /";
  match sess_user_id req with
  | None -> `Html Views.Index.html |> respond'
  | Some _ -> Uri.of_string "/channel/1" |> redirect' ~code:`See_other
end

let get_channel = get "/channel/:channel_id" begin fun req ->
  let channel_id = "channel_id" |> param req in
  print_endline ("GET /channel/" ^ channel_id);
  let user_id = sess_user_id req in
  match user_id, channel_id |> int_of_string with
  | None, _ -> Uri.of_string "/login" |> redirect' ~code:`See_other
  | Some user_id, channel_id ->
    let channels = M.Channel.all db in
    let channel =
      array_find_first channels (fun ch -> ch.id = Some channel_id) in
    match channel.description with
    | None -> assert false
    | Some desc ->
      let user = M.User.find db user_id in
      `Html (Views.Channel.html ~channels ~user desc) |> respond'
end

let get_register = get "/register" begin fun _ ->
  print_endline "GET /register";
  `Html Views.Register.html |> respond'
end

let post_register = post "/register" begin fun req ->
  print_endline "POST /register";
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    match form_value dict "name", form_value dict "password" with
    | name, password when name <> "" && password <> "" -> begin
        print_endline (
          "name:" ^ name  ^ ", " ^
          "password_length:" ^ (String.length password |> string_of_int) );
        assert (String.length name > 0);
        assert (String.length password > 0);
        let user_id = M.User.register db name password in
        Uri.of_string "/" |> redirect ~code:`See_other
        |> sess_set_user_id ~user_id
      end
    | _ ->
      no_content |> respond ~code:`Bad_request
    | exception _ ->
      no_content |> respond ~code:`Bad_request
  end
end

let get_login = get "/login" begin fun _ ->
  print_endline "GET /login";
  `Html Views.Login.html |> respond'
end

let post_login = post "/login" begin fun req ->
  print_endline "POST /login";
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    match form_value dict "name", form_value dict "password" with
    | name, password when name <> "" && password <> "" -> begin
        print_endline (
          "name:" ^ name  ^ ", " ^
          "password_length:" ^ (String.length password |> string_of_int) );
        assert (String.length name > 0);
        assert (String.length password > 0);
        let query = "SELECT * FROM user WHERE name = ?" in
        let params = [| `String name |] in
        let stmt, res =
          Db_helper.exec db query ~params () |> or_die "post_login" in
        match DB.Res.fetch (module DB.Row.Map) res |> or_die "post_login" with
        | Some row -> begin
            let user = M.User.get_from_row row in
            DB.Stmt.close stmt |> or_die "post_login";
            match user.id, user.salt, user.password with
            | Some user_id, Some salt, Some correct_digest ->
              let digest = calc_digest salt password in
              if digest <> correct_digest then
                no_content |> respond ~code:`Forbidden
              else
                Uri.of_string "/" |> redirect ~code:`See_other
                |> sess_set_user_id ~user_id
            | _ -> assert false
          end
        | None ->
          DB.Stmt.close stmt |> or_die "post_login";
          no_content |> respond ~code:`Forbidden
      end
    | _ ->
      no_content |> respond ~code:`Bad_request
    | exception _ ->
      no_content |> respond ~code:`Bad_request
  end
end

let get_logout = get "/logout" begin fun _ ->
  print_endline "GET /logout";
  redirect ~code:`See_other (Uri.of_string "/")
  |> Session.delete ~key:"user_id"
  |> Lwt.return
end

let post_message = post "/message" begin fun req ->
  print_endline "POST /message";
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    let user = ensure_login req in
    match form_value dict "message", form_value dict "channel_id" with
    | exception _ ->
      no_content |> respond ~code:`Forbidden
    | message, channel_id when message = "" || channel_id = "" ->
      no_content |> respond ~code:`Forbidden
    | message, channel_id ->
      print_endline "POST /message";
      print_endline begin
        "user_id:" ^ (
          match user.id with
          | Some id -> string_of_int id
          | None -> "Unknown"
        ) ^ ", "
        ^ "message:" ^ message ^ ", "
        ^ "channel_id:" ^ channel_id
      end;
      failwith "Not implemented"
  end
end

let get_message = get "/message" get_mock

let fetch_unread = get "/fetch" get_mock

let get_history = get "/history" get_mock

let get_profile = get "/profile/:user_name" get_mock

let get_add_channel = get "/add_channel" begin fun req ->
  print_endline "GET /add_channel";
  let user = ensure_login req in
  let channels = M.Channel.all db in
  `Html (Views.Add_channel.html ~channels ~user) |> respond'
end

let post_add_channel = post "/add_channel" begin fun req ->
  print_endline "POST /add_channel";
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    match form_value dict "name", form_value dict "description" with
    | exception _ ->
      no_content |> respond ~code:`Bad_request
    | name, desc when name = "" || desc = "" ->
      no_content |> respond ~code:`Bad_request
    | name, desc ->
      ensure_login req |> ignore;
      let last_id = M.Channel.insert db name desc in
      Printf.sprintf "/channel/%d" last_id
      |> Uri.of_string
      |> redirect ~code:`See_other
  end
end

let post_profile = post "/profile" begin fun req ->
  print_endline "POST /profile";
  App.urlencoded_pairs_of_body req |> Lwt.map begin fun dict ->
    post_mock dict
  end
end

let get_icon = get "/icon/:filename" get_mock

let main () =
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

let () = main ()