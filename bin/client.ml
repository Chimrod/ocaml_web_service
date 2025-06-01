open Lwt.Syntax

let root = "http://[::1]:8080"

let request =
  let* result =
    Cohttp_handler.request ~root
      (module Services_impl.Nb_car)
      () { value = "foobar" }
  in
  match result with
  | Error code ->
      prerr_endline ("Got code " ^ code);
      Lwt.return_unit
  | Ok { value; nbcar } ->
      print_endline
        (String.concat " "
           [
             "The number of characters for"; value; "is"; Int64.to_string nbcar;
           ]);
      Lwt.return_unit

let _ = Lwt_main.run request
