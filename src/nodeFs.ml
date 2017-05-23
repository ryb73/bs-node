(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

(** refernce documentation: https://nodejs.org/api/fs.html *)

(** Most fs functions let you omit the callback argument. If you do, a default
    callback is used that rethrows errors. To get a trace to the original call
    site, set the `NODE_DEBUG` environment variable:
*)
external readdirSync : string -> string array  = "" [@@bs.module "fs"]

external renameSync : string -> string = "" [@@bs.module "fs"]

type fd = private int

type path = string
(**
   The relative path to a filename can be used. Remember, however, that this path
   will be relative to [process.cwd()].
*)

module Watch = struct
  type t
  type config
  external config :
    ?persistent:bool ->
    ?recursive:bool ->
    ?encoding: Js_string.t ->
    unit -> config =
    "" [@@bs.obj]

  external watch :
    string ->
    ?config:config ->
    unit ->
    t = ""
  [@@bs.module "fs"]
  (** there is no need to accept listener, since we return a [watcher]
      back it can register event listener there .
      Currently we introduce a type [stringBuffer], for the
      [filename], it will be [Buffer] when the encoding is `utf8.
      This is dependent type which can be tracked by GADT in some way,
      but to make things simple, let's just introduce an or type
  *)
  external on :
    t
    -> ([
        | `change of (string (*eventType*) -> NodeStringBuffer.t (* filename *) -> unit  [@bs])
        | `error of (unit -> unit [@bs])
        ] [@bs.string])
    -> t = ""
  [@@bs.send]


  external close : t -> unit -> unit = "" [@@bs.send]
end

external ftruncateSync : fd -> int -> unit = "" [@@bs.module "fs"]

external truncateSync : string -> int -> unit = "" [@@bs.module "fs"]

external chownSync : string -> uid:int -> gid:int -> unit = "" [@@bs.module "fs"]

external fchownSync : fd -> uid:int -> gid:int -> unit = "" [@@bs.module "fs"]

external readlinkSync : string -> string  = "" [@@bs.module "fs"]

external unlinkSync : string -> unit  = "" [@@bs.module "fs"]

external rmdirSync : string -> unit = ""  [@@bs.module "fs"]

(* TODO: [flags] support *)
external openSync :
  path ->
  (
    [ `Read [@bs.as "r"]
    | `ReadWrite [@bs.as "r+"]
    | `ReadWriteSync [@bs.as "rs+"]
    | `Write [@bs.as "w"]
    | `WriteFailIfExists [@bs.as "wx"]
    | `WriteRead [@bs.as "w+"]
    | `WriteReadFailIfExists [@bs.as "wx+"]
    | `Append [@bs.as "a"]
    | `AppendFailIfExists [@bs.as "ax"]
    | `AppendRead [@bs.as "a+"]
    | `AppendReadFailIfExists [@bs.as "ax+"]
    ] [@bs.string]) ->
  unit = ""  [@@bs.module "fs"]


external readFileSync :
  string ->
  (
    [
      `hex
    | `utf8
    | `ascii
    | `latin1
    | `base64
    | `ucs2
    | `base64
    | `binary
    | `utf16le ][@bs.string]) ->
  string = "readFileSync"
[@@bs.val] [@@bs.module "fs"]

external readFileAsUtf8Sync :
  string -> (_[@bs.as "utf8"]) ->
  string = "readFileSync"
[@@bs.val] [@@bs.module "fs"]

external existsSync : string -> bool = ""
[@@bs.val] [@@bs.module "fs"]

external writeFileSync : filename:string -> text:string -> unit = ""
[@@bs.val] [@@bs.module "fs"]

external mkdirSync : string -> unit = "" [@@bs.val] [@@bs.module "fs"]

module type Typed = sig
  type t
end

module Stream = struct
  type 'a t = Stream of 'a

  let unbox (Stream s) = s

  external __end : 'a -> unit = "end" [@@bs.send]
  let _end s = __end @@ unbox s

  type error
  exception Exn of error
  external _on : 'a ->
    ([ `error of error -> unit
      | `open_ of unit -> unit [@bs.as "open"]
      | `close of unit -> unit
    ] [@bs.string]) -> unit = "on" [@@bs.send]

  let _onImpl s handler = match handler with
      | `error f -> _on s (`error f)
      | `open_ f -> _on s (`open_ f)
      | `close f -> _on s (`close f)

  let on (Stream s) = _onImpl s

  module Make(Typed : Typed) = struct
    let toStream (s : Typed.t) = Stream s
    let on (s : Typed.t) = _onImpl s
    let _end (s : Typed.t) = __end s
  end
end

module Writeable = struct
  type 'a t = Writeable of 'a

  let unbox (Writeable s) = s

  module Make(Typed : Typed) = struct
    include Stream.Make(Typed)

    let writeable (s : Typed.t) = Writeable s
  end
end

module Readable = struct
  type 'a t = Readable of 'a

  let unbox (Readable s) = s

  external _pipe : 'a -> 'b -> unit = "pipe" [@@bs.send]
  let pipe s w = _pipe (unbox s) (Writeable.unbox w)

  module Make(Typed : Typed) = struct
    include Stream.Make(Typed)

    let readable (s : Typed.t) = Readable s
    let pipe (s : Typed.t) w = _pipe s (Writeable.unbox w)
  end
end

module FileWriteStream = struct
  type t

  type t2 = t
  include Writeable.Make(struct
    type t = t2
  end)

  type createOptions = <
    flags           : string Js.undefined;
    defaultEncoding : string Js.undefined;
    fd              : int Js.undefined;
    mode            : int Js.undefined;
    autoClose       : bool Js.undefined;
    start           : int Js.undefined;
  > Js.undefined

  external _create : string -> createOptions -> t = "createWriteStream" [@@bs.module "fs"]

  let create path options scope =
    Js.Promise.make (fun ~resolve ~reject ->
      let stream = _create path options in

      on stream (`error (fun err ->
        reject (Stream.Exn err) [@bs]
      ));

      on stream (`open_ (fun () ->
        try
          ignore (
            scope stream
              |> Js.Promise.then_
                (fun () ->
                  let u = _end stream in
                  resolve u [@bs];
                  Js.Promise.resolve ()
                )
              |> Js.Promise.catch
                (fun err ->
                  (* prevent err from being cleaned up by compiler *)
                  ignore (Js.Promise.resolve err);

                  ignore ([%bs.raw {| reject(err) |}]);
                  Js.Promise.resolve ();
                )
          )
        with
          | err -> reject err [@bs]
      ));
    )
end

module FileReadStream = struct
  type t

  type t2 = t
  include Readable.Make(struct
    type t = t2
  end)

  type createOptions = (<
    flags     : string Js.undefined;
    encoding  : string Js.undefined;
    fd        : int Js.undefined;
    mode      : int Js.undefined;
    autoClose : bool Js.undefined;
    start     : int Js.undefined;
    _end      : int Js.undefined;
  > Js.t) Js.undefined

  external create : string -> createOptions -> t = "createReadStream" [@@bs.module "fs"]
end
