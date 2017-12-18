open Js.Result

external _randomBytes : int -> (Js.Exn.t Js.null -> Node.Buffer.t -> unit) -> unit = "randomBytes" [@@bs.module "crypto"]

let randomBytes size callback =
  _randomBytes size (fun optExn buffer ->
    let result = match (Js.Null.to_opt optExn) with
      | None -> Ok buffer
      | Some exn -> Error exn
    in
    callback result
  )