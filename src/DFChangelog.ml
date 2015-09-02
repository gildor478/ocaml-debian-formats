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

open DFUtils
open ExtString

type entry = 
    {
      source: string;
      version: string;
      distributions: string list;
      optional_fields: (string * string) list;
      urgency: string;
      maintainer: string;
      timestamp: string;
      changes: string;
    }

RE name_chars = ['-' '+' '0'-'9' 'a'-'z' '.']~

exception Skip_end 

(* Skip lines until it reachs the end 
 * of [ch]. Returns [Some _] if a matching
 * line is found and None if end is reached.
 *)
let skip_line ?fst ch = 
  let getl count f = 
    let l = 
      try 
        Some (IO.read_line ch)
      with IO.No_more_input ->
        None
    in
      match l with 
        | Some line -> f (count + 1) line
        | None -> count, None
  in

  let rec skip_line' count = 
    function
        (* Skip emacs variables, should be last line *)
        | RE bol (";;" space* )?"Local variables:"~ ->
            count, None

        (* Skip vim variables, should be last line *)
        | RE bol "vim:"~ ->
            count, None

        (* Skip CVS keyword *)
        | RE bol "# " ->
            getl count skip_line'

        (* Skip comments, even that's not supported *)
        | RE bol "/*" _* "*/" ->
            getl count skip_line'

        (* Blank line *)
        | RE bol space* eol ->
            getl count skip_line'

        | str ->
            count, Some str
  in
    match fst with 
      | Some line ->
          begin
            skip_line' 0 line
          end

      | None ->
          begin
            getl 0 skip_line'
          end

let parse_one ch fst = 
  let buff = 
    Buffer.create 13
  in

  let next ?fst where f =
    match skip_line ?fst ch with 
      | 0, Some line ->
          failwith 
            (Printf.sprintf 
               "Badly formatted %s line: '%s'"
               where line)
      | _, Some line ->
          f line
      | _, None ->
          failwith 
            (Printf.sprintf 
               "Unexpected end of file when parsing %s"
               where)
  in

  let parse_optional_fields str = 
    let lst = 
      (SPLIT space* "," space* ) str
    in
    let lst' =
      List.map
        (function
           | RE bol (['-' '0'-'9' 'a'-'z']+ as key) 
               "=" space*
               (_* as value) ~ ->
              key, value
           | str ->
               failwith 
                 (Printf.sprintf 
                    "Badly formatted optional field '%s'"
                    str))
        lst
    in
    let is_urgency (k,_) = 
      String.lowercase k = "urgency"
    in
    let _, urgency = 
      try 
        List.find is_urgency lst'
      with Not_found ->
        failwith 
          "Cannot find urgency field"
    in
      urgency, (List.filter (fun e -> not (is_urgency e)) lst')
  in

  let rec _parse_keep line =
    (* TODO *)
    ()

  and parse_header line = 
    match line with 
      (* Regex header *)
      | RE bol (alpha name_chars* as source) 
          " (" (_* as version) ")" 
          ((space+ name_chars+)* as distribs)
          ";" space* (_* as extra_fields) ->
          let maintainer, timestamp = 
            next "changes/trailer" parse_change_or_trailer
          in
          let urgency, optional_fields =
            parse_optional_fields extra_fields
          in
          let distributions =
            (SPLIT space+) distribs
          in
          let changes = 
            Buffer.contents buff
          in
            Buffer.clear buff;
            {
              source          = source;
              version         = version;
              distributions   = distributions;
              optional_fields = optional_fields;
              urgency         = urgency;
              maintainer      = maintainer;
              timestamp       = timestamp;
              changes         = changes;
            }

      | _ ->
          next "header" ~fst:line parse_header

  and parse_change_or_trailer line =
    match line with
      | RE bol space{2} (_* as str) ->
          Buffer.add_string buff str;
          next "changes/trailer" parse_change_or_trailer

      (* Regex trailer *)
      | RE bol " -- " (_* as maint) 
          space+
          "<" (_* as mail) ">" 
          space+ 
          ((alpha+ "," space* )?
            digit{1-2} space+ alpha+ space+ digit{4}  
            space+ digit{1-2} ":" digit digit ":" digit digit  
            space+ ['-''+'] digit{4} 
            (space+ "(" _ ")")? as timestamp) 
          space* ->
          (Printf.sprintf "%s <%s>" maint mail), timestamp

      | _ ->
          next "changes/trailer" ~fst:line parse_change_or_trailer
  in
    parse_header fst

(** Only parse the first entry 
  *)
let head ch = 
  match skip_line ch with 
    | _, Some line ->
        parse_one ch line
    | _, None ->
        failwith "No first changelog entry"

(** Parse the full changelog 
  *)
let parse ch = 
  let rec parse_aux lst = 
    match skip_line ch with 
      | _, Some line -> 
          parse_aux ((parse_one ch line) :: lst)
      | _, None ->
          List.rev lst
  in
    parse_aux []

let to_string e = 
  let buff = 
    Buffer.create 13
  in
  let add fmt =
    Printf.ksprintf 
      (fun s -> 
         Buffer.add_string buff s;
         Buffer.add_char buff '\n')
      fmt 
  in
    add "%s (%s) %s; %s"
      e.source 
      e.version 
      (String.concat " " e.distributions)
      (String.concat ", " 
         (List.map 
            (fun (k,v) ->
               Printf.sprintf "%s=%s" k v)
            (("urgency", e.urgency) :: e.optional_fields)));
    add "";
    List.iter 
      (add "  %s")
      (String.nsplit e.changes "\n");
    add "";
    add " -- %s %s" e.maintainer e.timestamp;
    Buffer.contents buff

let filename =
  Filename.concat "debian" "changelog"

let default () = 
  with_fn
    filename
    parse

let default_head () = 
  with_fn
    filename
    head
