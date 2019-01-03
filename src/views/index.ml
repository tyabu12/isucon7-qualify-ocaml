let html =
  let open Tyxml in
  [%html {|
<h1>ようこそ Isubata へ。</h1>
<p>Isubata は世界最速のチャットアプリケーションです。</p>
<p>利用を開始するには<a href="/login">ログイン</a>してください。</p>
<p>アカウントをお持ちでない場合は<a href="/register">新規登録</a>してください。</p>
  |}] |> Layout.html