open Cohttp_lwt_unix
open Lwt.Syntax

let url_of_string : V_string.t -> string = Obj.magic

let code_of_response : Response.t -> (int, string) Result.t =
 fun resp ->
  let code = resp |> Response.status |> Cohttp.Code.code_of_status in
  match Cohttp.Code.is_success code with
  | true -> Ok 200
  | false -> Error (string_of_int code)

let get_method : type a b. (a, b) Services.method_ -> Http.Method.t = function
  | POST -> `POST
  | GET -> `GET
  | PUT -> `PUT
  | DELETE -> `DELETE
  | PATCH -> `PATCH
  | HEAD -> `HEAD
  | CONNECT -> `CONNECT
  | OPTIONS -> `OPTIONS
  | TRACE -> `TRACE

(** Encodde the response given by cohttp for the service *)
let map_response :
    (module Services.JsonClientHandler
       with type request = 'request
        and type response = 'response
        and type placeholders = 'placeholders) ->
    Cohttp_lwt.Body.t ->
    'response Lwt.t =
 fun (type request response placeholders)
     (module S : Services.JsonClientHandler
       with type request = request
        and type response = response
        and type placeholders = placeholders) body ->
  match S.method_ with
  | GET | POST | PUT | DELETE | PATCH | CONNECT | OPTIONS | TRACE ->
      let* body_content = Cohttp_lwt.Body.to_string body in
      let json = Ppx_yojson_conv_lib.Yojson.Safe.from_string body_content in
      Lwt.return (S.response_of_yojson json)
  | HEAD -> Lwt.return_unit

let request :
    root:string ->
    (module Services.JsonClientHandler
       with type request = 'request
        and type response = 'response
        and type placeholders = 'placeholders) ->
    'placeholders ->
    'request ->
    ('response, string) result Lwt.t =
 fun (type request response placeholders) ~root
     (module S : Services.JsonClientHandler
       with type request = request
        and type response = response
        and type placeholders = placeholders) parameters request ->
  let uri =
    V_string.concat ~sep:(V_string.v "/")
      [ V_string.v root; Path.build parameters S.path ]
  in

  (* There is no body for GET or HEAD method *)
  let request_body =
    match S.method_ with
    | Services.GET | Services.HEAD -> None
    | _ ->
        Some
          (request |> S.yojson_of_request
         |> Ppx_yojson_conv_lib.Yojson.Safe.to_string
         |> Cohttp_lwt.Body.of_string)
  in

  let uri = Uri.of_string (url_of_string uri) in
  let* response, body =
    Client.call ?headers:None ?body:request_body (get_method S.method_) uri
  in

  match code_of_response response with
  | Error _ as e -> Lwt.return e
  | Ok _ ->
      let* response_body = map_response (module S) body in
      Lwt.return_ok response_body
