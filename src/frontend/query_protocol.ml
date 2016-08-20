(* {{{ COPYING *(

  This file is part of Merlin, an helper for ocaml editors

  Copyright (C) 2013 - 2015  Frédéric Bour  <frederic.bour(_)lakaban.net>
                             Thomas Refis  <refis.thomas(_)gmail.com>
                             Simon Castellan  <simon.castellan(_)iuwt.fr>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  The Software is provided "as is", without warranty of any kind, express or
  implied, including but not limited to the warranties of merchantability,
  fitness for a particular purpose and noninfringement. In no event shall
  the authors or copyright holders be liable for any claim, damages or other
  liability, whether in an action of contract, tort or otherwise, arising
  from, out of or in connection with the software or the use or other dealings
  in the Software.

)* }}} *)

open Std

module Compl =
struct
  type 'desc raw_entry = {
    name: string;
    kind: [`Value|`Constructor|`Variant|`Label|
           `Module|`Modtype|`Type|`MethodCall];
    desc: 'desc;
    info: 'desc;
  }

  type entry = string raw_entry

  type application_context = {
    argument_type: string;
    labels : (string * string) list;
  }

  type t = {
    entries: entry list;
    context: [ `Unknown
             | `Application of application_context
             ]
  }
end

type completions = Compl.t

type outline = item list
and item = {
  outline_name : string ;
  outline_kind : [
    | `Value
    | `Constructor
    | `Label
    | `Module
    | `Modtype
    | `Type
    | `Exn
    | `Class
    | `Method
  ];
  location : Location_aux.t ;
  children : outline ;
}

type shape = {
  shape_loc : Location_aux.t;
  shape_sub : shape list;
}

type is_tail_position = [`No | `Tail_position | `Tail_call]

type _ t =
  | Type_expr
    :  string * Msource.position
    -> string t
  | Type_enclosing
    :  (string * int) option * Msource.position
    -> (Location.t * string * is_tail_position) list t
  | Enclosing
    :  Msource.position
    -> Location.t list t
  | Complete_prefix
    :  string * Msource.position * bool
    -> completions t
  | Expand_prefix
    :  string * Msource.position
    -> completions t
  | Document
    : string option * Msource.position
    -> [ `Found of string
       | `Invalid_context
       | `Builtin of string
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `No_documentation
       ] t
  | Locate
    : string option * [ `ML | `MLI ] * Msource.position
    -> [ `Found of string option * Lexing.position
       | `Invalid_context
       | `Builtin of string
       | `Not_in_env of string
       | `File_not_found of string
       | `Not_found of string * string option
       | `At_origin
       ] t
  | Jump
    : string * Msource.position
    -> [ `Found of Lexing.position
       | `Error of string
       ] t
  | Case_analysis
    : Msource.position * Msource.position -> (Location.t * string) t
  | Outline
    :  outline t
  | Shape
    :  Msource.position
    -> shape list t
  | Errors
    :  Location.error list t
  | Dump
    :  json list
    -> json t
  | Which_path
    :  string list
    -> string t
  | Which_with_ext
    :  string list
    -> string list t
  | Flags_get
    :  string list t
  | Findlib_list
    :  string list t
  | Extension_list
    :  [`All|`Enabled|`Disabled]
    -> string list t
  | Path_list
    :  [`Build|`Source]
    -> string list t
  | Project_get
    :  (string list * [`Ok | `Failures of (string * exn) list]) t
  | Occurrences
    : [`Ident_at of Msource.position]
    -> Location.t list t
  | Version
    : string t
