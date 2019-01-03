module M = Models

let html ?channel_id ?(channels =[||]) ?user contents =
  let open Tyxml in
  let open Printf in
  let nav_li_channel =
    match channel_id with
    | Some channel_id ->
      [[%html {|
        <li class="nav-item">
          <a href="|} (sprintf "/history/%d" channel_id)
                {|" class="nav-link">チャットログ</a>
        </li>
      |}]]
    | None -> []
  in
  let nav_li_user =
    match user with
    | None -> [
        [%html {|<li><a href="/register" class="nav-link">新規登録</a></li>|}];
        [%html {|<li><a href="/login" class="nav-link">ログイン</a></li>|}];
      ]
    | Some user -> M.User.(
      match user.name, user.display_name with
      | Some user_name, Some user_display_name -> [
          [%html {|
            <li class="nav-item">
              <a href="/add_channel" class="nav-link">チャンネル追加</a>
            </li>
          |}]; [%html {|
            <li class="nav-item">
              <a href="|} ("/profile/" ^ user_name) {|" class="nav-link">
                |} [Html.txt user_display_name] {|
              </a>
            </li>
          |}]; [%html {|
            <li class="nav-item">
              <a href="/logout" class="nav-link">ログアウト</a>
            </li>
          |}]
        ]
      | _ -> assert false
    )
  in
  let nav_channels =
    match user with
    | None -> []
    | Some _ ->
      let li_channels =
        Array.fold_right begin fun channel li_list -> M.Channel.(
          match channel.id, channel.name with
          | Some cid, Some cname ->
            let active = if Some cid = channel_id then "active" else "" in
            [%html {|
              <li class="nav-item">
                <a class=|} ["nav-link"; "justify-content-between"; active] {|
                  href=|} (sprintf "/channel/%d" cid) {|>
                  |} [Html.txt cname] {|
                  <span class="badge badge-pill badge-primary float-right"
                        id=|} (sprintf "unread-%d" cid) {|></span>
                </a>
              </li>
            |}]
          | _ -> assert false
        ) :: li_list
        end channels [] in
      [[%html {|<ul class="nav nav-pills flex-column">|} li_channels {|</ul>|}]]
  in
  [%html {|
<!DOCTYPE html>
<html>
  <head>
    <meta http-equiv="Content-Type" content="text/html" charset="utf-8">
    <title>Isubata</title>
    <link rel="stylesheet" href="/css/bootstrap.min.css">
    <link rel="stylesheet" href="/css/main.css">
    <script src="/js/jquery.min.js"> </script>
    <script src="/js/tether.min.js"> </script>
    <script src="/js/bootstrap.min.js"> </script>
  </head>
  <body>

    <nav class="navbar navbar-toggleable-md navbar-inverse fixed-top bg-inverse">
      <button class="navbar-toggler navbar-toggler-right hidden-lg-up" type="button" data-toggle="collapse" data-target="#navbarsExampleDefault" aria-controls="navbarsExampleDefault" aria-expanded="false" aria-label="Toggle navigation">
        <span class="navbar-toggler-icon"></span>
      </button>
      <a class="navbar-brand" href="/">Isubata</a>

      <div class="collapse navbar-collapse" id="navbarsExampleDefault">
        <ul class="nav navbar-nav ml-auto">
          |} (nav_li_channel @ nav_li_user) {|
        </ul>
      </div>
    </nav>

	<div class="container-fluid">
  <div class="row">
    <nav class="col-sm-3 col-md-3 hidden-xs-down bg-faded sidebar">
      |} nav_channels {|
    </nav>
    <main class="col-sm-9 offset-sm-3 col-md-9 offset-md-3 pt-3">
      |} contents {|
    </main>
  </div>
	</div>

  </body>
</html>
  |}] |> Format.asprintf "%a" (Html.pp ())