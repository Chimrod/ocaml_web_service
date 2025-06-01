type t = string

let v : string -> t = Fun.id
let s : t -> string = Fun.id
let concat : ?sep:t -> t list -> t = fun ?(sep = "") -> StringLabels.concat ~sep
let encode = Dream.to_percent_encoded ?international:None
