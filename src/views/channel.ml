let html ~channels ~user description =
  let open Tyxml in
  let%html user_content = {|
    <div class="row">
      <div class="col-sm-9 col-md-9" id="chatbox-frame">
        <div class="input-group chatbox">
          <textarea class="form-control" rows="3"  id="chatbox-textarea"> </textarea>
          <span class="input-group-btn"> <button class="btn btn-primary" onclick="on_send_button()">送信</button> </span>
        </div>
      </div>
    </div>
  |} in
  [%html {|
<div class="well">|}[Html.txt description]{|</div>
<div id="timeline"></div>
|}[ if user = None then Html.txt "" else user_content ]{|
<script src="/js/chat.js"> </script>
  |}] |> Layout.html ~channels ?user