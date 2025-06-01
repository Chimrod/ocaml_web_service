type ('a, 'b) method_ =
  | GET : (unit, 'b) method_
  | POST : ('a, 'b) method_
  | PUT : ('a, 'b) method_
  | DELETE : ('a, 'b) method_
  | HEAD : (unit, unit) method_
  | CONNECT : ('a, 'b) method_
  | OPTIONS : ('a, 'b) method_
  | TRACE : ('a, 'b) method_
  | PATCH : ('a, 'b) method_

(** A simple service, with an input and and answer *)
module type Handler = sig
  type request
  (** The body of the request *)

  type response
  (** The body of the response *)

  val method_ : (request, response) method_
  (** The method used in the service. *)

  type placeholders
  (** Parameters given in the url path. This type will match variable parts in
      the path to the service *)

  val path : placeholders Path.t
  (** Path to the service *)
end

(** The service implemented in the server:

    We need to be able to decode the content and encode the response. *)
module type JsonServerHandler = sig
  include Handler

  val request_of_yojson : Yojson.Safe.t -> request
  (** Extract the request elements from the json *)

  val yojson_of_response : response -> Yojson.Safe.t
  (** Produce a json from the response given by the service *)
end

module type JsonClientHandler = sig
  include Handler

  val yojson_of_request : request -> Yojson.Safe.t
  val response_of_yojson : Yojson.Safe.t -> response
end
