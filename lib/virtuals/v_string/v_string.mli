type t
(**
   This module provide a common signature between the server and javascript
   side in order to create URL in the same way.
*)

val v : string -> t
val s : t -> string
val concat : ?sep:t -> t list -> t

val encode : t -> t
(** Encode an element of the url by percent-encoding. The function is
      expected to encode "/" as well.
   *)
