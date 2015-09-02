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

open Curl
open Lwt

let with_fn_out fn f = 
  let chn = open_out fn in
    try 
      let res = f chn in
        close_out chn;
        res
    with e ->
      close_out chn;
      raise e
      
let with_fn_in fn f = 
  let chn = open_in fn in
    try 
      let res = f chn in
        close_in chn;
        res
    with e ->
      close_in chn;
      raise e

let with_curl f =
  let c = Curl.init () in
    try 
      let res = f c in
        Curl.cleanup c;
        res
    with e ->
      Curl.cleanup c;
      raise e


module ODBMessage =
struct
  let info ~ctxt fmt = 
    Printf.ksprintf
      (fun str -> return (prerr_endline ("I: "^str)))
      fmt

  let debug ~ctxt fmt =
    Printf.ksprintf
      (fun str -> return (prerr_endline ("D: "^str)))
      fmt

  let warning ~ctxt fmt = 
    Printf.ksprintf
      (fun str -> return (prerr_endline ("W: "^str)))
      fmt
end

module S = 
struct 
  let use sqle f =
    f ()

  let execute () fmt =
    Printf.ksprintf 
      (fun str -> 
         Printf.eprintf "SQL: %s\n" str; 
         return ())
      fmt

  let transaction () f =
    f ()
end

module ODBOASIS =
struct

  let from_string ~ctxt ?fn str = 
    let ctxt = 
      {!OASISContext.default with 
           OASISContext.
           ignore_plugins = true;
           ignore_unknown_fields = true}
    in
      OASISParse.from_string ~ctxt ?fn str
end

module Context =
struct 
  type t = 
      {
        sqle : unit;
      }

  let default = 
    {
      sqle = ();
    }
end

module ODBCurl = 
struct 
  let download_if_new url fn = 

    let fn_etag = fn^".etag" in 

    let cur_etag = 
      if Sys.file_exists fn then
        begin
          try 
            let chn = 
              open_in fn_etag
            in
            let res = 
              String.make (in_channel_length chn) '\000'
            in
              really_input chn res 0 (String.length res);
              close_in chn;
              Some res
          with _ ->
            None
        end
      else
        None
    in

    let etag_regex = 
      ignore "(*";
      Pcre.regexp "ETag: \"(.*)\""
    in

    let write_fun chn str =
      output_string chn str;
      String.length str
    in

    let header_fun str =
      let () = 
        try 
          let substr = 
            Pcre.exec ~rex:etag_regex str
          in
          let etag =
            Pcre.get_substring substr 1
          in
            with_fn_out fn_etag
              (fun chn ->
                 output_string chn etag)

        with Not_found ->
          ()
      in
        String.length str
    in

    let fn_tmp = 
      fn^".tmp"
    in
      catch 
        (fun () -> 
           with_fn_out fn_tmp
             (fun chn ->
                with_curl
                  (fun c ->
                     Curl.set_url c url;
                     Curl.set_followlocation c true;
                     begin
                       match cur_etag with 
                         | Some etag ->
                             Curl.set_httpheader c 
                               [Printf.sprintf 
                                  "If-None-Match: \"%s\""
                                  etag]
                         | None ->
                             ()
                     end;
                     Curl.set_headerfunction c header_fun;
                     Curl.set_writefunction c (write_fun chn);
                     Curl.perform c;
                     match Curl.get_httpcode c with 
                       | 200 ->
                           return true
                       | 304 ->
                           return false
                       | n ->
                           fail 
                             (Failure 
                                (Printf.sprintf "Unexpected HTTP code %d" n))))
             >|= fun is_new ->
             begin
               if is_new then 
                 begin
                   (* TODO: lwt version of this *)
                   FileUtil.rm [fn];
                   FileUtil.mv fn_tmp fn;
                 end
               else
                 begin
                   FileUtil.rm [fn_tmp]
                 end;
               is_new
             end)

        (fun e ->
           FileUtil.rm [fn; fn_tmp; fn_etag];
           fail e)
end

open ExtLib
open DebianFormats

type uri = string
type filename = string

type t = 
    {
      deb_force:  bool;
      deb_mirror: uri;
      deb_dist:   string;
      deb_distro: string;
      deb_tmpdir: filename;
    }

module SetString = Set.Make(String)

let download_uri ~ctxt uri fn = 
  ODBMessage.info ~ctxt "Start downloading '%s'" uri
  >>= fun () ->
  ODBCurl.download_if_new uri fn
  >>= fun res ->
  ODBMessage.info ~ctxt "End downloading '%s'" uri
  >|= fun () ->
  res

let download_uri_exists ~ctxt uri fn =
  if Sys.file_exists fn then
    return ()
  else
    begin
      download_uri ~ctxt uri fn
      >>= fun (is_new : bool) ->
      return ()
    end

let find_fn bn fn = 
  ArchiveLwt.Read.create (`Filename fn)
  >>= fun arch ->
  begin
    let rec find lst = 
      try 
        List.find 
          (fun fn -> 
             not (ArchiveLwt.Read.is_directory arch fn) &&
             FilePath.basename fn = bn) 
          lst
      with 
        | Not_found when lst <> [] ->
            let lst = 
              List.fold_left
                (fun acc fn ->
                   if ArchiveLwt.Read.is_directory arch fn then
                     begin
                       let lst' = 
                         List.rev_map 
                           (FilePath.concat fn)
                           (Array.to_list (ArchiveLwt.Read.readdir arch fn))
                       in
                         List.rev_append lst' acc
                     end
                   else
                     acc)
                []
                lst
            in
              find lst
        | e ->
            raise e
    in
      try 
        begin
          let fn = 
            find (Array.to_list (ArchiveLwt.Read.readdir arch ""))
          in
            ArchiveLwt.Read.content arch fn 
            >|= fun ctnt ->
            Some ctnt
        end

      with Not_found ->
        begin
          return None
        end
  end

(* Update available package list *)
let update_packages ~ctxt t pkg_lst = 
  Lwt_list.fold_left_s 
    (fun acc e -> 
       try 
         let fn, _, _ = 
           Source.filename e `Tarball 
         in
         let uri = URI.pool t.deb_mirror e fn in
         let tarball_fn = 
           FilePath.make_filename
             [t.deb_tmpdir;
              "pool";
              FilePath.add_extension
                (e.Source.name ^"-"^ (Version.upstream e.Source.version) ^ ".tar")
                (FilePath.get_extension fn)]
         in
           download_uri_exists ~ctxt uri tarball_fn
           >>= fun () ->
           find_fn "_oasis" tarball_fn 
           >|= 
             function
               | Some oasis_ctnt ->
                   let oasis =
                     ODBOASIS.from_string ~ctxt oasis_ctnt 
                   in
                     (`Sure (oasis, e)) :: acc
               | None ->
                   (`Unsure e) :: acc

       with Not_found ->
         ODBMessage.warning ~ctxt 
           "No tarball found for %s" 
           e.Source.name
         >|= fun () ->
        acc)
    []
    pkg_lst
    >>= fun inject_pkg ->
    (* TODO: fix unsure packages when they have already been dealt with *)
    S.use ctxt.Context.sqle
      (fun db ->
         S.transaction db
           (fun () ->
              S.execute db 
                "DELETE FROM distro_pkg WHERE distro = %s" t.deb_distro
              >>= fun () ->
              S.execute db 
                "DELETE FROM distro_pkg_unsure WHERE distro = %s" t.deb_distro
              >>= fun () ->
              Lwt_list.iter_s
                (function
                   | `Sure (oasis, e) ->
                       Lwt_list.iter_s
                         (fun (bin, _) ->
                            S.execute db
                              "INSERT INTO distro_pkg(distro, distro_src, distro_bin, pkg, ver) \
                               VALUES (%s, %s, %s, %s, %s)"
                              t.deb_distro 
                              e.Source.name
                              bin
                              (oasis.OASISTypes.name)
                              (OASISVersion.string_of_version                                  
                                 oasis.OASISTypes.version))
                         e.Source.binary
                   | `Unsure e ->
                       Lwt_list.iter_s
                         (fun (bin, _) ->
                            S.execute db
                              "INSERT INTO distro_pkg_unsure(distro, distro_src, distro_bin, ver) \
                               VALUES (%s, %s, %s, %s)"
                              t.deb_distro
                              e.Source.name
                              bin
                              (Version.upstream e.Source.version))
                         e.Source.binary)
                inject_pkg))

(** Update watch files *)
let update_watches ~ctxt t pkg_lst =
  Lwt_list.iter_s 
    (fun e ->
       try 
         let fn, _, _ = 
           Source.filename e `Diff
         in
           if String.ends_with fn ".debian.tar.gz" then
             begin
               let uri = URI.pool t.deb_mirror e fn in
               let diff_fn = 
                 FilePath.make_filename
                   [t.deb_tmpdir;
                    "pool";
                    e.Source.name ^"-"^ (Version.noepoch e.Source.version) ^ ".debian.tar.gz"]
               in
                 download_uri_exists ~ctxt uri diff_fn 
                 >>= fun () ->
                 find_fn "watch" diff_fn 
                 >|= 
                   function
                     | Some watch_ctnt ->
                         (* TODO: inject *)
                         prerr_endline watch_ctnt
                     | None ->
                         ()
             end
           else
             return ()

       with Not_found ->
         ODBMessage.debug ~ctxt "No diff found for %s" e.Source.name)
    pkg_lst

let update ~ctxt t = 
  let fn = 
    Filename.concat t.deb_tmpdir (t.deb_distro^"-sources")
  in
  let fn_bz2 = 
    fn^".bz2"
  in

    download_uri ~ctxt (URI.sources t.deb_mirror t.deb_dist `Main) fn_bz2 
    >>= fun is_new ->
    (* TODO: build a list of extra packages *)
    return SetString.empty
    >>= fun extra_set ->
    (* TODO: build a package of ignored packages *)
    return SetString.empty
    >>= fun ignore_set ->
    begin
      if is_new || t.deb_force then
        begin
          ODBMessage.info ~ctxt "File '%s' is new" fn_bz2
          >>= fun () ->
          (* Decompress Source.bz2 *)
          Lwt_preemptive.detach 
            (fun () ->
               with_fn_in fn_bz2
                 (fun chn_bz2 ->
                    with_fn_out fn
                      (fun chn_out ->
                         let chn_bz2' = Bz2.open_in chn_bz2 in
                           try 
                             let len = 4096 in
                             let buf = String.make len '\000' in
                             let read () = Bz2.read chn_bz2' buf 0 len in
                             let byte_read = ref (read ()) in
                               while !byte_read = len do 
                                 output_string chn_out buf;
                                 byte_read := read ()
                               done;
                               output chn_out buf 0 !byte_read;
                               Bz2.close_in chn_bz2'
                           with e ->
                             Bz2.close_in chn_bz2';
                             raise e)))
            ()
          >>= fun () ->
          (* Extract ocaml packages from Sources *)
          Lwt_preemptive.detach 
            (fun () ->
               with_fn_in 
                 fn
                 (fun chn ->
                    let rlst = 
                      ref []
                    in
                    let _i: unit list = 
                      Source.parse 
                        (fun e ->
                           if (e.Source.section = "ocaml" || 
                               SetString.mem e.Source.name extra_set) &&
                              not (SetString.mem e.Source.name ignore_set) then
                             rlst := e :: !rlst)
                        (IO.input_channel chn)
                    in
                      !rlst))
            ()
          >>= fun lst ->
          update_packages ~ctxt t lst
          >>= fun () ->
          update_watches ~ctxt t lst
        end
      else
        ODBMessage.info ~ctxt "File '%s' has not changed" fn_bz2
    end

let () = 
  let () =
    Curl.global_init CURLINIT_GLOBALNOTHING
  in

  let default = 
    {
      deb_force  = true;
      deb_mirror = "http://ftp.debian.org/debian";
      deb_dist   = "unstable";
      deb_distro = "debian-unstable";
      deb_tmpdir = "tmp/";
    }
  in
    Lwt_main.run
      (update ~ctxt:Context.default default);
    Curl.global_cleanup ()
