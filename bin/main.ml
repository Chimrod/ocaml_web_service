(** Create the handler for the service *)
let handler =
  Dream_handler.handle
    (module Services_impl.Nb_car)
    (fun (() : Services_impl.Nb_car.placeholders) body ->
      Lwt.return_ok
        Services_impl.Nb_car.
          {
            value = body.value;
            nbcar = Int64.of_int (String.length body.value);
          })

(* The handler and the route are not created at the same time because we may
   want create a specific handler, for example one checking CRSF in the query
   and can’t infer this from the service signature only *)

(** And create the route. *)
let route = Dream_handler.register (module Services_impl.Nb_car) handler

(** Generate a default static page *)
let hello : Dream.handler =
 fun _ ->
  Dream.html
    {|<html>
  <body>
  <h1>Hello!</h1>
  <div>
      <noscript>Sorry, you need to enable JavaScript to see this page.</noscript>
      <script id="lib" type="text/javascript" defer="defer" src="js/content.js"></script>
      <script>
        var script = document.getElementById('lib');
        lib.addEventListener('load', function() {
            client.start()
        })
      </script>
  </div>
  <div id="content" />
  </body>
</html>|}

let js_assets _root path _request =
  (* This module is automatically generated — see the dune file to see the rule *)
  match JsAssets.read path with
  | None -> Dream.empty `Not_Found
  | Some asset -> Dream.respond asset

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/js/**" (Dream.static ~loader:js_assets "");
         Dream.get "/" hello;
         route;
       ]

(* Now test the application by connecting to 
    http://localhost:8080/
 *)
