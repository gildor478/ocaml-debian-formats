
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
  ]


let _ = 
  run_test_tt_main tests
