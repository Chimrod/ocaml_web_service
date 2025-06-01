(** Describe the components in the path *)
type _ typ =
  | Int : int64 typ
  | String : string typ
  | Fixed : V_string.t -> unit typ

(** [get_param f] extract the value from the poositional argument.

    The function [f] call the server engine for the value *)
let get_param : type a. int -> (string -> string) -> a typ -> a =
 fun idx f -> function
  | Fixed _ -> ()
  | String -> f (string_of_int idx)
  | Int -> Int64.of_string (f (string_of_int idx))

let repr_param : type a. int -> a typ -> V_string.t =
 fun idx -> function
  | String | Int -> V_string.v (":" ^ string_of_int idx)
  | Fixed v -> v

let encode_param : type a. a -> a typ -> V_string.t =
 fun value -> function
  | String -> V_string.v value
  | Int -> V_string.v (Int64.to_string value)
  | Fixed v -> v

type _ t =
  | [] : unit t
  | ( :: ) : 'x t * 'y t -> ('x * 'y) t
  | T1 : 'a typ -> 'a t
  | T2 : 'a typ * 'b typ -> ('a * 'b) t
  | T3 : 'a typ * 'b typ * 'c typ -> ('a * 'b * 'c) t
  | T4 : 'a typ * 'b typ * 'c typ * 'd typ -> ('a * 'b * 'c * 'd) t

(** Count the number of arguments. *)
let rec count : type a. a t -> int = function
  | [] -> 0
  | T1 _ -> 1
  | T2 _ -> 2
  | T3 _ -> 3
  | T4 _ -> 4
  | p1 :: p2 -> count p1 + count p2

(** Extract the arguments from the path template *)
let unzip : 'a t -> (string -> string) -> 'a =
 fun t f ->
  let rec _unzip : type a. int -> a t -> a =
   fun idx path ->
    let extract idx p = get_param idx f p in
    match path with
    | [] -> ()
    | p1 :: tl -> (_unzip idx p1, _unzip (idx + count p1) tl)
    | T1 p1 -> extract idx p1
    | T2 (p1, p2) -> (extract idx p1, extract (idx + 1) p2)
    | T3 (p1, p2, p3) ->
        (extract idx p1, extract (idx + 1) p2, extract (idx + 2) p3)
    | T4 (p1, p2, p3, p4) ->
        ( extract idx p1,
          extract (idx + 1) p2,
          extract (idx + 2) p3,
          extract (idx + 3) p4 )
  in
  _unzip 0 t

let repr : 'a t -> V_string.t =
 fun t ->
  let rec _repr : type a. int -> V_string.t list -> a t -> V_string.t list =
   fun idx acc t ->
    match t with
    | [] -> acc
    | T1 t -> repr_param idx t :: acc
    | T2 (t1, t2) -> repr_param idx t1 :: repr_param (idx + 1) t2 :: acc
    | T3 (t1, t2, t3) ->
        repr_param idx t1
        :: repr_param (idx + 2) t2
        :: repr_param (idx + 3) t3
        :: acc
    | T4 (t1, t2, t3, t4) ->
        repr_param idx t1
        :: repr_param (idx + 1) t2
        :: repr_param (idx + 2) t3
        :: repr_param (idx + 3) t4
        :: acc
    | tx :: ty ->
        let idx' = count tx in
        let acc' = _repr idx' acc ty in
        _repr idx acc' tx
  in
  let args = _repr 0 [] t in
  V_string.concat ~sep:(V_string.v "/") args

let repr' a = repr a |> V_string.s

let build : 'a -> 'a t -> V_string.t =
 fun parameters uri ->
  ignore (parameters, uri);
  let rec _build : type a. V_string.t list -> a -> a t -> V_string.t list =
   fun acc value path ->
    match path with
    | [] -> acc
    | T1 t -> encode_param value t :: acc
    | T2 (t1, t2) ->
        let v1, v2 = value in
        encode_param v1 t1 :: encode_param v2 t2 :: acc
    | T3 (t1, t2, t3) ->
        let v1, v2, v3 = value in
        encode_param v1 t1 :: encode_param v2 t2 :: encode_param v3 t3 :: acc
    | T4 (t1, t2, t3, t4) ->
        let v1, v2, v3, v4 = value in
        encode_param v1 t1 :: encode_param v2 t2 :: encode_param v3 t3
        :: encode_param v4 t4 :: acc
    | tx :: ty ->
        let vx, vy = value in
        _build (_build acc vy ty) vx tx
  in
  V_string.concat ~sep:(V_string.v "/") (_build [] parameters uri)
