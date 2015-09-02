(******************************************************************************)
(*  ocaml-fileutils: files and filenames common operations                    *)
(*                                                                            *)
(*  Copyright (C) 2003-2014, Sylvain Le Gall                                  *)
(*                                                                            *)
(*  This library is free software; you can redistribute it and/or modify it   *)
(*  under the terms of the GNU Lesser General Public License as published by  *)
(*  the Free Software Foundation; either version 2.1 of the License, or (at   *)
(*  your option) any later version, with the OCaml static compilation         *)
(*  exception.                                                                *)
(*                                                                            *)
(*  This library is distributed in the hope that it will be useful, but       *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of                *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file         *)
(*  COPYING for more details.                                                 *)
(*                                                                            *)
(*  You should have received a copy of the GNU Lesser General Public License  *)
(*  along with this library; if not, write to the Free Software Foundation,   *)
(*  Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA             *)
(******************************************************************************)

type entry = {
  source : string;
  version : string;
  distributions : string list;
  optional_fields : (string * string) list;
  urgency : string;
  maintainer : string;
  timestamp : string;
  changes : string;
}
exception Skip_end
val __mikmatch_regexp_1 : Pcre.regexp
val __mikmatch_regexp_2 : Pcre.regexp
val __mikmatch_regexp_3 : Pcre.regexp
val __mikmatch_regexp_4 : Pcre.regexp
val __mikmatch_regexp_5 : Pcre.regexp
val skip_line : ?fst:string -> IO.input -> int * string option
val __mikmatch_regexp_6 : Pcre.regexp
val __mikmatch_regexp_7 : Pcre.regexp
val __mikmatch_regexp_8 : Pcre.regexp
val __mikmatch_regexp_9 : Pcre.regexp
val __mikmatch_regexp_10 : Pcre.regexp
val __mikmatch_regexp_11 : Pcre.regexp
val parse_one : IO.input -> string -> entry
val head : IO.input -> entry
val parse : IO.input -> entry list
val to_string : entry -> string
val filename : string
val default : unit -> entry list
val default_head : unit -> entry
