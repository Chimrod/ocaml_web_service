module Make (S : sig
  type t
end) =
struct
  module State = S

  module type Processor = sig
    type t

    val process : t -> S.t -> S.t
  end

  module ID : Processor with type t = unit = struct
    type t = unit

    let process () state = state
  end

  type event = E : 'a * (module Processor with type t = 'a) -> event

  (** Simple helper for the main event loop *)
  let run : ?eq:(S.t -> S.t -> bool) -> S.t -> event Note.E.t -> S.t Note.S.t =
   fun ?eq init event ->
    let action =
      Note.E.map (fun (E (t, (module P))) st -> P.process t st) event
    in
    Note.S.accum ?eq init action

  let dispatch : (module Processor with type t = 's) -> 's -> event =
   fun (type s) (module P : Processor with type t = s) v -> E (v, (module P))
end
