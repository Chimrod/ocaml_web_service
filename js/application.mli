(** The Make module build the main application loop.contents

    The function [run] update the state on each event, and return a new state.
    Each event must follow the [event] type, which is composed from the type
    [t], and a module with a fonction [update].

    This example create an application with the state containing a simple
    counter. An even which increment this counter is created and can be used to
    update the state.

    {[
      type state = { value : int }

      (** Increment the state. *)
      module Incr = struct
          type t = unit

          let process () state = { value = state.value + 1 }
      end

      (** Decrement the state. *)
      module Incr = struct
          type t = unit

          let process () state = { value = state.value - 1 }
      end

      module App = Make(struct type t = state end)

      (* Create the event processor *)
      let incr_event = App.dispatch (module Incr) ()
      and decr_event = App.dispatch (module Decr) () in

      let init = { value = 0 } in

      (* Run the main loop *)
      let state = App.run
        init
        (E.select
          [ incr_event
          ; decr_event ] ) in …
    ]} *)
module Make (S : sig
  type t
end) : sig
  module type Processor = sig
    type t

    val process : t -> S.t -> S.t
  end

  module ID : Processor with type t = unit

  type event

  val dispatch : (module Processor with type t = 's) -> 's -> event
  (** [dispatch (module P) v] will create an event holding a value [v] and
      associated with the processor [P] *)

  val run : ?eq:(S.t -> S.t -> bool) -> S.t -> event Note.E.t -> S.t Note.S.t
  (** The function [run state ev] will create a signal continually updated each
      time the event [ev] occur *)
end
