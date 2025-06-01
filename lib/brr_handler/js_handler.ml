let get_method : type a b. (a, b) Services.method_ -> Jstr.t = function
  | GET -> Jstr.v "GET"
  | POST -> Jstr.v "POST"
  | PUT -> Jstr.v "PUT"
  | DELETE -> Jstr.v "DELETE"
  | HEAD -> Jstr.v "HEAD"
  | CONNECT -> Jstr.v "CONNECT"
  | OPTIONS -> Jstr.v "OPTIONS"
  | TRACE -> Jstr.v "TRACE"
  | PATCH -> Jstr.v "PATCH"

let send :
    (module Services.JsonClientHandler
       with type request = 'request
        and type response = 'response
        and type placeholders = 'placeholders) ->
    'placeholders ->
    'request ->
    ('response, Jv.Error.t) Fut.result =
 fun (type request response placeholders)
     (module S : Services.JsonClientHandler
       with type request = request
        and type response = response
        and type placeholders = placeholders) parameters request ->
  let json_repr =
    S.yojson_of_request request |> Ppx_yojson_conv_lib.Yojson.Safe.to_string
  in
  let body =
    match S.method_ with
    | GET | HEAD -> None
    | _ -> Some (Brr_io.Fetch.Body.of_jstr (Jstr.of_string json_repr))
  in
  let init =
    Brr_io.Fetch.Request.init ?body ~method':(get_method S.method_) ()
  in

  (* There is no way to retreive the type of the string from the virtual module. 
     I know the type match, but I have to tell this to the compiler…
   *)
  let url_of_string : V_string.t -> Jstr.t = Obj.magic in
  let url' : Jstr.t = url_of_string @@ Path.build parameters S.path in
  let response = Brr_io.Fetch.(request @@ Request.v ~init url') in

  (* Now handle the response *)
  let open Fut.Result_syntax in
  let* content = response in
  let body = Brr_io.Fetch.Response.as_body content in
  let* str_body = Brr_io.Fetch.Body.text body in
  let str = Jstr.to_string str_body in
  try
    let json = Ppx_yojson_conv_lib.Yojson.Safe.from_string str in
    Result.Ok (S.response_of_yojson json) |> Fut.return
  with Yojson.Json_error err -> Fut.error (Jv.Error.v (Jstr.v err))
