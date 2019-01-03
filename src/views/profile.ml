let html ~channel_id ~channels ~user ~other ~self_profile =
  let open Tyxml in
  begin
    if self_profile then begin
      match Models.User.(user.name, user.display_name, user.avatar_icon) with
      | Some name, Some display_name, Some avatar_icon -> [[%html {|
<form action="/profile" method="post" enctype="multipart/form-data">
  <div class="form-group row">
    <label class="col-sm-2 col-form-label">ユーザ名</label>
    <div class="col-sm-10"> <p>|} [Html.txt name] {|</p> </div>

    <label class="col-sm-2 col-form-label">表示名</label>
    <div class="col-sm-10"> <input type="text" class="form-control" name="display_name" placeholder="表示名" value=|} display_name {|> </div>

    <label class="col-sm-2 col-form-label">アイコン</label>
    <div class="col-sm-10"> <input type="file" name="avatar_icon"> </div>

    <div class="col-sm-2"></div>
    <div class="col-sm-10">
      <img class="avatar-lg"
           src=|} ("/icons/" ^ avatar_icon) {|
           alt="no avatar">
    </div>
  </div>

  <button type="submit" class="btn btn-primary">更新</button>
</form>
        |}]]
      | _ -> assert false
    end else
      match Models.User.(other.name, other.display_name, other.avatar_icon) with
      | Some name, Some display_name, Some avatar_icon -> [[%html {|
<div class="form-group row">
  <label class="col-sm-2 col-form-label">ユーザ名</label>
  <div class="col-sm-10"> <p>|} [Html.txt name] {|<p> </div>

  <label class="col-sm-2 col-form-label">表示名</label>
  <div class="col-sm-10"> <p>|} [Html.txt display_name] {|<p> </div>

  <label class="col-sm-2 col-form-label">アイコン</label>
  <div class="col-sm-10">
    <img class="avatar-lg" src=|} ("/icons/" ^ avatar_icon) {| alt="no avatar">
  </div>
</div>
    |}]]
      | _ -> assert false
  end |> Layout.html ~channel_id ~channels ~user