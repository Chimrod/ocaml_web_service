type t = Jstr.t

let v : string -> t = Jstr.v
let s = Jstr.to_string
let concat : ?sep:t -> t list -> t = Jstr.concat

let encode v =
  match Brr.Uri.encode_component v with
  | Ok s -> s
  | Error _ -> Jstr.empty
