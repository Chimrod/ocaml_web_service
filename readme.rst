.. -*- mode: rst -*-
.. -*-  coding: utf-8 -*-

This is a sample project showing how to process http requests in a safe way in
OCaml.

For example:

    - A service using the GET method cannot have any body resquest
    - A service using the HEAD method cannot have any body response

Interface
=========

The interface for a service is given in an OCaml module, and is declaring all
the possibles values for a service.

Each service shall describe:

- an URL to the server
- the body of the request
- the body of the response

.. code:: ocaml

    type request = { value : string } [@@deriving yojson]

    type response = { value : string; nbcar : int64 } [@@deriving yojson]

    (** The method used in the service *)
    let method_ = Services.POST

    type placeholders = unit
    (** No placeholder here in the request url *)

    (** The path to the service, matching the type parameters *)
    let path = Path.(T1 (Fixed (V_string.v "api/counter")))

Here, we are not declaring any implementation, only the types.

Implementation
==============

The server part
---------------


There is no implementation yet, this is done in the server, for example using
`Dream_handler`:

.. code:: ocaml

    let handler =
      Dream_handler.handle
        (module Services_impl.Nb_car)
        (fun (() : Services_impl.Nb_car.placeholders) body ->
          Lwt.return_ok
            Services_impl.Nb_car.
              {
                value = body.value;
                nbcar = Int64.of_int (String.length body.value);
              })

As the url is also given in the service interface, we can also create the route:

.. code:: ocaml

    let route = Dream_handler.register (module Services_impl.Nb_car) handler

    let () =
      Dream.run @@ Dream.logger
      @@ Dream.router
           [
             route;
           ]


The client part
---------------

This example use the library brr to make a request to this service:

.. code:: ocaml

    let futr = Js_handler.send (module Services_impl.Nb_car) () { value }

Another example using cohttp :

.. code:: ocaml

  let root = "http://[::1]:8080"
  let* result =
    Cohttp_handler.request ~root
      (module Services_impl.Nb_car)
      () { value = "foobar" }
