val handle :
  (module Services.JsonServerHandler
     with type placeholders = 'placeholders
      and type request = 'request
      and type response = 'response) ->
  ('placeholders -> 'request -> ('response, string) Lwt_result.t) ->
  'placeholders ->
  Dream.handler
(** [handle (module S) f] create a handler for the requests. 

    @arg f is the function receiving the variable parts of the url, the body
    (matching the type S.request), the request (in order to fetch the
    parameters) and returning a content of type S.response.

    The function does not read any parameters from the URI, as the body is
    supposed having all the required informations.

 *)

module MakeChecked (S : Services.JsonServerHandler) : sig
  exception Invalid_method
  (** Exception raised if the method does not allow body content *)

  val handle :
    (S.placeholders -> S.request -> (S.response, string) Lwt_result.t) ->
    S.placeholders ->
    Dream.handler
end

val register :
  ?path:'placeholders Path.t ->
  (module Services.JsonServerHandler with type placeholders = 'placeholders) ->
  ('placeholders -> Dream.handler) ->
  Dream.route
(** Register a handler as a route. The module gives all the required information
    (path, methods…) we need, and the handler can be created using the function
    `handle` just above.

    {[
      let handler =
        Dream_handler.handle
          (module Service)
          (fun (_args : Service.parameters) (_body : Services.request) ->
            Lwt.return_ok _)

      let route = Route_builder.register (module Service) handler
    ]}*)
