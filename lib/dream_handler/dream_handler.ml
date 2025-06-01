open Lwt_result.Syntax

(** Extract the content from the body request.

    The module given in argument is the definition of the service. *)
let read_body :
    (module Services.JsonServerHandler with type request = 'request) ->
    Dream.request ->
    ('request, Dream.response) result Lwt.t =
 fun (type request)
     (module S : Services.JsonServerHandler with type request = request)
     request ->
  let%lwt json =
    match S.method_ with
    | GET | HEAD ->
        (* GET and HEAD method doesn’t have any body. We assume here the body
             is typed as Unit *)
        Lwt.return `Null
    | _ ->
        let%lwt body = Dream.body request in
        Yojson.Safe.from_string body |> Lwt.return
  in
  let json_content = Lwt.return @@ S.request_of_yojson json in
  Lwt_result.ok json_content

let create_response :
    (module Services.JsonServerHandler with type response = 'response) ->
    ('response, Dream.response) result Lwt.t ->
    Dream.response Dream.promise =
 fun (type response)
     (module S : Services.JsonServerHandler with type response = response)
     response_content ->
  let response =
    let* response_content = response_content in
    let yojson_content = S.yojson_of_response response_content in
    Yojson.Safe.to_string yojson_content |> Lwt_result.return
  in
  match%lwt response with Ok json -> Dream.json json | Error e -> Lwt.return e

(** Simple handler which read the content and apply the transformations to the
    response. *)
let handle :
    (module Services.JsonServerHandler
       with type placeholders = 'placeholders
        and type request = 'request
        and type response = 'response) ->
    ('placeholders -> 'request -> ('response, string) Lwt_result.t) ->
    'placeholders ->
    Dream.handler =
 fun (type placeholders request response)
     (module S : Services.JsonServerHandler
       with type placeholders = placeholders
        and type response = response
        and type request = request) f args request ->
  let response =
    let* body = read_body (module S) request in
    Lwt_result.map_error
      (fun e -> Dream.response ~status:`Internal_Server_Error e)
      (f args body)
  in
  create_response (module S) response

module MakeChecked (S : Services.JsonServerHandler) = struct
  exception Invalid_method

  (** Derive the handler from the standard one by adding a new field [token] in
      the request *)
  module Service = struct
    include S
    open Ppx_yojson_conv_lib.Yojson_conv.Primitives

    type ('a, 'b) result = ('a, 'b) Result.t = Ok of 'a | Error of 'b
    [@@deriving yojson]

    type request = { content : S.request; token : string }
    [@@deriving of_yojson]
    (** This type add the validation token in the body message *)

    let method_ : (request, response) Services.method_ =
      match S.method_ with
      (* We can’t add the crsf token with thoses methods because they do not
        have body *)
      | GET | HEAD -> raise Invalid_method
      | POST -> POST
      | PUT -> PUT
      | DELETE -> DELETE
      | CONNECT -> CONNECT
      | OPTIONS -> OPTIONS
      | TRACE -> TRACE
      | PATCH -> PATCH
  end

  let check_token :
      Dream.request -> string -> (unit, Dream.response) Lwt_result.t =
   fun request token ->
    match%lwt Dream.verify_csrf_token request token with
    | `Ok -> Lwt.return_ok ()
    | _ -> Lwt_result.fail (Dream.response ~status:`Unauthorized "")

  (** Override the handle function by checking the token validity *)
  let handle :
      (S.placeholders -> S.request -> (S.response, string) Lwt_result.t) ->
      S.placeholders ->
      Dream.handler =
   fun f args request ->
    let response =
      let* content = read_body (module Service) request in

      (* Extract the token from the body and check the validity *)
      let* () = check_token request content.token in

      Lwt_result.map_error
        (fun e -> Dream.response ~status:`Internal_Server_Error e)
        (f args content.content)
    in
    create_response (module Service) response
end

let extract_param request name =
  Dream.param request name |> Dream.from_percent_encoded

let method' : type a b.
    (a, b) Services.method_ -> string -> Dream.handler -> Dream.route = function
  | GET -> Dream.get
  | PUT -> Dream.put
  | POST -> Dream.post
  | DELETE -> Dream.delete
  | HEAD -> Dream.head
  | CONNECT -> Dream.connect
  | OPTIONS -> Dream.options
  | TRACE -> Dream.trace
  | PATCH -> Dream.patch

(** Handle the given URL encoded in the application.

    Use the type system to ensure that the path for the route will use the same
    arguments name as in the extraction, and that this url will match the
    signature for the handler

    [handle] method ?path url handler

    will call the [handler] with the arguments extracted from [url]. If [path]
    is given, the route will be created against [path] instead, which allow to
    use differents url inside a scope. *)
let register :
    ?path:'placeholders Path.t ->
    (module Services.JsonServerHandler with type placeholders = 'placeholders) ->
    ('a -> Dream.handler) ->
    Dream.route =
 fun (type placeholders) ?path
     (module S : Services.JsonServerHandler
       with type placeholders = placeholders) f ->
  let partial_handler =
    (* There is no unification possible for the type p' and p when both are
       available.
       That’s why need to evaluate it now in order to remove any abstraction as
       soon as possible *)
    match path with
    | None -> method' S.method_ Path.(repr' S.path)
    | Some p' -> method' S.method_ Path.(repr' p')
  in

  partial_handler (fun request ->
      let placeholders = Path.unzip S.path (extract_param request) in
      f placeholders request)
