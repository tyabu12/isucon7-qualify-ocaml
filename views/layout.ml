let html contents =
  let open Tyxml in
  let%html page = {|
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
        </ul>
      </div>
    </nav>

	<div class="container-fluid">
  <div class="row">
      <main class="col-sm-9 offset-sm-3 col-md-9 offset-md-3 pt-3">
        |}contents{|
    </main>
  </div>
	</div>

  </body>
</html>
|} in
  Format.asprintf "%a" (Html.pp ()) page