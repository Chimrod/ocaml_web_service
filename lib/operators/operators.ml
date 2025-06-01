module type T = sig
  type 'a t

  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module DefaultIter (T : sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
end) =
struct
  let iter f v = ignore (T.map f v)
end

module type MONAD = sig
  type 'a t

  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Binding (T : T) = struct
  (* Create the let binding operators for a module.

     - let* is the binding operator we can find in a monad.
     - let/ is the map operator
     - let- is the operator which reduce to unit. (I wanted to use . as symbol
       like in caqti, but this is not allowed )

     The list of the symbols used is describe here :
         https://v2.ocaml.org/manual/bindingops.html#start-section
  *)

  let ( let* ) : 'a T.t -> ('a -> 'b T.t) -> 'b T.t = T.bind
  let ( let+ ) : 'a T.t -> ('a -> 'b) -> 'b T.t = fun t f -> T.map f t
  let ( let- ) : 'a T.t -> ('a -> unit) -> unit = fun t f -> T.iter f t
end

module type Traversable = sig
  type 'a t

  (** 

  Build the traversable module. 

  [>] means that the parameter is not wrapped in the MONAD
  [**] means that the function is returning both a MONAD and the type
  [*] means that the function binding into a new MONAD

  The name is choosen in order to make sense in the successive binding. You
  should have the first binding using [let>…] form, then [let_], and finally
  just [let] 

   *)
  module Make (T : MONAD) : sig
    val ( let>** ) : 'a t -> ('a -> 'b t T.t) -> 'b t T.t
    val ( let>* ) : 'a t -> ('a -> 'b T.t) -> 'b t T.t
    val ( let** ) : 'a t T.t -> ('a -> 'b t T.t) -> 'b t T.t
    val ( let* ) : 'a t T.t -> ('a -> 'b T.t) -> 'b t T.t
  end
end

module TraversableResult : Traversable with type 'a t = ('a, string) result =
struct
  type 'a t = ('a, string) result

  module Make (T : MONAD) = struct
    let traverse : 'a t -> ('a -> 'b T.t) -> 'b t T.t =
     fun v f ->
      match v with
      | Ok x -> T.map (fun x -> Ok x) (f x)
      | Error e -> T.return (Error e)

    let ( let>* ) : 'a t -> ('a -> 'b T.t) -> 'b t T.t = traverse

    let ( let>** ) : 'a t -> ('a -> 'b t T.t) -> 'b t T.t =
     fun v f ->
      let result = traverse v (fun v -> f v) in
      T.map Result.join result

    let ( let* ) : 'a t T.t -> ('a -> 'b T.t) -> 'b t T.t =
     fun v f -> T.bind v (fun v -> traverse v f)

    let ( let** ) : 'a t T.t -> ('a -> 'b t T.t) -> 'b t T.t =
     fun v f ->
      T.bind v (fun v ->
          let result = traverse v (fun v -> f v) in
          T.map Result.join result)
  end
end
