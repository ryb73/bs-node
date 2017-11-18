external _randomBytes : int -> (Js.Exn.t Js.null -> NodeBuffer.t -> unit) -> unit = "randomBytes" [@@bs.module "crypto"]

let randomBytes size callback =
  _randomBytes size (fun optExn buffer ->
    let result = match (Js.Null.to_opt optExn) with
      | None -> `Buffer buffer
      | Some exn -> `Exception exn
    in
    callback result
  )