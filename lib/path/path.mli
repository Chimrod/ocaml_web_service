(** Describe the components in the path *)
type _ typ =
  | Int : int64 typ
  | String : string typ
  | Fixed : V_string.t -> unit typ

type _ t =
  | [] : unit t
  | ( :: ) : 'x t * 'y t -> ('x * 'y) t
  | T1 : 'a typ -> 'a t
  | T2 : 'a typ * 'b typ -> ('a * 'b) t
  | T3 : 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) t
  | T4 : 'a typ * 'b typ * 'c typ * 'd typ -> ('a * 'b * 'c * 'd) t

val unzip : 'a t -> (string -> string) -> 'a
val repr : 'a t -> V_string.t
val repr' : 'a t -> string
val build : 'a -> 'a t -> V_string.t
