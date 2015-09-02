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

open OUnit
open DebianFormats

let with_fn fn f = 
  bracket 
    (fun () ->
       open_in fn)
    (fun chn ->
       f (IO.input_channel chn))
    (fun chn ->
       close_in chn)

let tests =
  "DebianFormats">:::
  ["Control">:::
   (List.map 
      (fun (fn, f) ->
         fn >::
         with_fn fn
           (fun ch ->
              f (Control.parse ch)))
      [
        "control.ocaml-data-notation",
        (fun (src, binaries) ->
           assert_equal 
             ~printer:(fun s -> s)
             "ocaml-data-notation"
             src.Control.source)
      ]);

   "Changelog">:::
   (List.map 
      (fun (fn, f) ->
         fn >::
         with_fn fn
           (fun ch ->
              f (Changelog.head ch)))
      [
        "changelog.ocaml-data-notation",
        (fun e ->
           assert_equal 
             ~msg:"source"
             ~printer:(fun s -> s)
             "ocaml-data-notation"
             e.Changelog.source;
           assert_equal
             ~msg:"version"
             ~printer:(fun s -> s)
             "0.0.3-1"
             e.Changelog.version);
      ]);

   "Watch">:::
   (List.map 
      (fun (fn, f) ->
         fn >::
         with_fn fn
           (fun ch ->
              f (Watch.parse ch)))
      [
        "watch.oasis",
        (fun lst ->
           List.iter prerr_endline lst);
        "watch.obus",
        (fun lst ->
           List.iter prerr_endline lst);
      ]);
  ]


let _ = 
  run_test_tt_main tests
