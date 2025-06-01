module OptionInfix = Operators.Binding (Option)

let add_field :
    label:Jstr.t ->
    id':Jstr.t ->
    value':Jstr.t ->
    Brr.El.t list ->
    Brr.El.t list =
 fun ~label ~id' ~value' elt ->
  Brr.El.label ~at:Brr.At.[ for' id' ] [ Brr.El.txt label ]
  :: Brr.El.input ~at:Brr.At.[ type' (Jstr.v "text"); id id'; value value' ] ()
  :: elt

module State = struct
  type t = { word : string; len : int; counter : int }

  let repr_html : t -> Brr.El.t list =
   fun { word; len; counter } ->
    [
      Brr.El.form
      @@ add_field ~id':(Jstr.v "text_state") ~label:(Jstr.v "Word received")
           ~value':(Jstr.v word)
      @@ add_field ~id':(Jstr.v "nbcar_state") ~label:(Jstr.v "Nb of car")
           ~value':(Jstr.of_int len)
      @@ add_field ~id':(Jstr.v "counter_state") ~label:(Jstr.v "Request sent")
           ~value':(Jstr.of_int counter) [];
    ]
end

(** Service transforming the response from the request into the state *)
module WordCount = struct
  type t = Services_impl.Nb_car.response

  let process response state =
    Brr.Console.log
      [
        Jstr.v response.Services_impl.Nb_car.value;
        Int64.to_int response.Services_impl.Nb_car.nbcar;
      ];
    State.
      {
        counter = state.counter + 1;
        word = response.Services_impl.Nb_car.value;
        len = Int64.to_int response.Services_impl.Nb_car.nbcar;
      }
end

module App = Application.Make (State)

let main () =
  let open OptionInfix in
  let- content_div =
    Brr.Document.find_el_by_id Brr.G.document (Jstr.v "content")
  in

  let form =
    Brr.El.form
      [
        Brr.El.label
          ~at:Brr.At.[ for' (Jstr.v "text") ]
          [ Brr.El.txt (Jstr.v "Text") ];
        Brr.El.input
          ~at:
            Brr.At.
              [
                type' (Jstr.v "text"); id (Jstr.v "text"); name (Jstr.v "text");
              ]
          ();
        Brr.El.input
          ~at:Brr.At.[ type' (Jstr.v "submit"); value (Jstr.v "Count") ]
          ();
      ]
  in

  (* Listen the submit event on the form. This is an example of event of event :

     First we listen for the click event, and then for the request response
     event. *)
  let post_event : Brr.El.t -> App.event Note.event =
   fun form ->
    Note.E.join
    @@ Note_brr.Evr.on_el Brr_io.Form.Ev.submit
         (fun ev ->
           (* Do not send the query, we use it with javascript *)
           Brr.Ev.prevent_default ev;

           (* Extract the data from the form *)
           let data = Brr_io.Form.(Data.of_form (of_el form)) in
           let text_value = Brr_io.Form.Data.find data (Jstr.v "text") in
           let value =
             match text_value with
             | Some (`String s) -> Jstr.to_string s
             | _ -> ""
           in

           (* Send the request *)
           Js_handler.send (module Services_impl.Nb_car) () { value }
           |> Note_brr.Futr.to_event
           |> Note.E.map (function
                | Error _ -> App.dispatch (module App.ID) ()
                | Ok response -> App.dispatch (module WordCount) response))
         form
  in
  let bottom = Brr.El.div [] in
  Brr.El.append_children content_div [ form; Brr.El.hr (); bottom ];

  let state =
    App.run
      { word = ""; len = 0; counter = 0 }
      (Note.E.select [ post_event form ])
  in

  Note_brr.Elr.def_children bottom (Note.S.map State.repr_html state);

  let log state = ignore state in
  Note.Logr.hold (Note.S.log state log)

let () =
  Brr.Console.(debug [ Jstr.v "Js started" ]);
  let open Jv in
  let post = obj [| ("start", repr main) |] in
  set global "client" post
