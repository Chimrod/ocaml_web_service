(** Service counting the characters in a word *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type request = { value : string } [@@deriving yojson]
type response = { value : string; nbcar : int64 } [@@deriving yojson]

(** The method used in the service *)
let method_ = Services.POST

type placeholders = unit
(** No placeholder here in the request url *)

(** The path to the service, matching the type parameters *)
let path = Path.(T1 (Fixed (V_string.v "api/counter")))
