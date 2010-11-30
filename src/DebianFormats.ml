(**************************************************************************************)
(*  Copyright (C) 2009 Pietro Abate <pietro.abate@pps.jussieu.fr>                     *)
(*  Copyright (C) 2009 Mancoosi Project                                               *)
(*                                                                                    *)
(*  This library is free software: you can redistribute it and/or modify              *)
(*  it under the terms of the GNU Lesser General Public License as                    *)
(*  published by the Free Software Foundation, either version 3 of the                *)
(*  License, or (at your option) any later version.  A special linking                *)
(*  exception to the GNU Lesser General Public License applies to this                *)
(*  library, see the COPYING file for more information.                               *)
(**************************************************************************************)

open DF822
open ExtLib
open DFUtils

type name = string
type version = string
type vpkg = (string * (string * string) option)
type veqpkg = (string * (string * string) option)
type architecture = string

(**/**)
let default dflt f1 f2 fld = 
  try 
    f1 f2 fld
  with Not_found -> 
    dflt

(**/**)

module Release = 
struct 
  type t = 
      {
        origin : string;
        label : string;
        suite : string;
        version: string;
        codename : string;
        date: string;
        architecture: string;
        component : string;
        description: string;
        md5sums: (string * string * string) list;
        sha1: (string * string * string) list;
        sha256: (string * string * string) list
      }

  let parse ch =
    let parse_release_fields par =
      let parse field =
        try (single_line field (List.assoc field par))
        with Not_found -> ""
      in
        {
          origin       = parse "origin";
          label        = parse "label";
          suite        = parse "suite";
          version      = parse "version";
          codename     = parse "codename";
          date         = parse "date";
          architecture = parse "architecture";
          component    = parse "component";
          description  = parse "description";
          md5sums      = [];
          sha1         = [];
          sha256       = [];
        }
    in
      match parse_paragraph (start_from_channel ch) with
      | None -> 
          raise Not_found
      | Some par -> 
          parse_release_fields par
end

module Source = 
struct 
  type t = 
      {
        name : name;
        version : version;
        binary : vpkg list;
        build_depends : (vpkg * (bool * architecture) list) list list;
        build_depends_indep : (vpkg * (bool * architecture) list) list list;
        build_conflicts : (vpkg * (bool * architecture) list) list;
        build_conflicts_indep : (vpkg * (bool * architecture) list) list;
        architecture : architecture list
      }

  let parse_name = parse_package
  let parse_arch s = Str.split (Str.regexp " ") s
  let parse_version s = parse_version s
  let parse_binary s = parse_vpkglist parse_constr s
  let parse_cnf s = parse_vpkgformula parse_builddeps s
  let parse_conj s = parse_vpkglist parse_builddeps s

  (* Relationships between source and binary packages
   * http://www.debian.org/doc/debian-policy/ch-relationships.html
   * Build-Depends, Build-Depends-Indep, Build-Conflicts, Build-Conflicts-Indep
  *)
  let parse_sources_fields par =
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let exec () =
      {
        name                  = parse_s parse_name    "package";
        version               = parse_s parse_version "version";
        architecture          = parse_s parse_arch    "architecture";
        binary                = default [] parse_m parse_binary  "binary";
        build_depends         = default [] parse_m parse_cnf  "build-depends";
        build_depends_indep   = default [] parse_m parse_cnf  "build-depends-indep";
        build_conflicts       = default [] parse_m parse_conj "build-conflicts";
        build_conflicts_indep = default [] parse_m parse_conj "build-conflicts-indep";
      }
    in
      try 
        Some (exec ()) 
      with Not_found -> 
        None (* this package doesn't either have version, arch or name *)

  (** parse a debian Sources file from channel *)
  let parse ch =
    let parse_packages = 
      parse_822_iter parse_sources_fields 
    in
      parse_packages 
        (fun i -> i)
        (start_from_channel ch)
end

module Binary = 
struct 
  (** debian package format *)
  type t = 
      {
        name : name ;
        version : version;
        essential : bool;
        source : (name * version option) ;
        depends : vpkg list list;
        pre_depends : vpkg list list;
        recommends : vpkg list list;
        suggests : vpkg list;
        enhances : vpkg list;
        conflicts : vpkg list;
        breaks : vpkg list;
        replaces : vpkg list;
        provides : veqpkg list;
        extras : (string * string) list;
      }

  let parse_name = parse_package
  let parse_vpkg = parse_constr
  let parse_veqpkg = parse_constr 
  let parse_conj s = parse_vpkglist parse_vpkg s 
  let parse_cnf s = parse_vpkgformula parse_vpkg s 
  let parse_prov s = parse_veqpkglist parse_veqpkg s 
  let parse_essential = function
    |("Yes"|"yes") -> true
    |("No" | "no") -> false (* this one usually is not there *)
    |_ -> assert false (* unreachable ?? *)

  let parse_packages_fields extras par =
    let extras = "status"::extras in
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let parse_e extras =
      List.filter_map 
        (fun prop -> 
           let prop = String.lowercase prop 
           in
             try 
               Some (prop,single_line prop (List.assoc prop par))
             with Not_found -> 
               None) 
        extras
    in

    let exec () = 
      {
        name        = parse_s parse_name "package";
        version     = parse_s parse_version "version";
        essential   = default false parse_s parse_essential "essential";
        source      = default ("",None) parse_s parse_source "source";
        depends     = default [] parse_m parse_cnf  "depends";
        pre_depends = default [] parse_m parse_cnf  "pre-depends";
        recommends  = default [] parse_m parse_cnf  "recommends";
        suggests    = default [] parse_m parse_conj "suggests";
        enhances    = default [] parse_m parse_conj "enhances";
        conflicts   = default [] parse_m parse_conj "conflicts";
        breaks      = default [] parse_m parse_conj "breaks";
        replaces    = default [] parse_m parse_conj "replaces";
        provides    = default [] parse_m parse_prov "provides";
        extras      = parse_e extras;
      }
    in
      (* this package doesn't either have version or name *)
      try 
        Some(exec ()) 
      with Not_found -> 
        None 

  (** parse a debian Packages file from the channel [ch] *)
  let parse ?(extras=[]) f ch =

    let parse_packages = 
      parse_822_iter 
        (parse_packages_fields extras) 
    in
      parse_packages f (start_from_channel ch)
end

module Control =
struct

  (** debian source section format *)
  type source_section = 
      {
        source : name;
        section: name;
        priority: name;
        maintainer: string;
        uploaders: string list; 
        standards_version: version;
        build_depends : (vpkg * (bool * architecture) list) list list;
        build_depends_indep : (vpkg * (bool * architecture) list) list list;
        build_conflicts : (vpkg * (bool * architecture) list) list;
        build_conflicts_indep : (vpkg * (bool * architecture) list) list;
      }

  (** debian binary sections format *)
  type binary_section = 
      {
        package : name;
        essential : bool;
        depends : vpkg list list;
        pre_depends : vpkg list list;
        recommends : vpkg list list;
        suggests : vpkg list;
        enhances : vpkg list;
        conflicts : vpkg list;
        breaks : vpkg list;
        replaces : vpkg list;
        provides : veqpkg list;
        extras : (string * string) list;
      }

  type t = source_section * binary_section list

  let parse_name = parse_package
  let parse_vpkg = parse_virtual_constr
  let parse_veqpkg = parse_virtual_constr
  let parse_conj s = parse_vpkglist parse_vpkg s 
  let parse_essential = function
    |("Yes"|"yes") -> true
    |("No" | "no") -> false (* this one usually is not there *)
    |_ -> assert false (* unreachable ?? *)

  let parse_source_fields par = 
    let parse_cnf s = parse_vpkgformula parse_builddeps s in
    let parse_conj s = parse_vpkglist parse_builddeps s in
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let exec () =
      {
        source                = parse_s parse_name "source";
        section               = parse_s parse_name "section"; (* TODO: be more precise *)
        priority              = parse_s parse_name "priority"; (* TODO: more precise *)
        maintainer            = ""; (* TODO *)
        uploaders             = []; (* TODO *) 
        standards_version     = parse_s parse_version "standards-version";
        build_depends         = default [] parse_m parse_cnf  "build-depends";
        build_depends_indep   = default [] parse_m parse_cnf  "build-depends-indep";
        build_conflicts       = default [] parse_m parse_conj "build-conflicts";
        build_conflicts_indep = default [] parse_m parse_conj "build-conflicts-indep";
      }
    in
      try 
        Some (exec ()) 
      with Not_found -> 
        None (* this package doesn't either have version, arch or name *)

  let parse_binary_fields extras par =
    let extras = "status"::extras in
    let parse_s f field = f (single_line field (List.assoc field par)) in
    let parse_m f field = f (String.concat " " (List.assoc field par)) in
    let parse_cnf s = parse_vpkgformula parse_vpkg s in
    let parse_prov s = parse_veqpkglist parse_veqpkg s in
    let parse_e extras =
      List.filter_map 
        (fun prop -> 
           let prop = String.lowercase prop 
           in
             try 
               Some (prop,single_line prop (List.assoc prop par))
             with Not_found -> 
               None) 
        extras
    in

    let exec () = 
      {
        package     = parse_s parse_name "package";
        essential   = default false parse_s parse_essential "essential";
        (* TODO: handle ${ ... } *)
        depends     = (try default [] parse_m parse_cnf  "depends" with _ -> []);
        pre_depends = default [] parse_m parse_cnf  "pre-depends";
        recommends  = default [] parse_m parse_cnf  "recommends";
        suggests    = default [] parse_m parse_conj "suggests";
        enhances    = default [] parse_m parse_conj "enhances";
        conflicts   = default [] parse_m parse_conj "conflicts";
        breaks      = default [] parse_m parse_conj "breaks";
        replaces    = default [] parse_m parse_conj "replaces";
        (* TODO: handle ${ ... } *)
        provides    = (try default [] parse_m parse_prov "provides" with _ -> []);
        extras      = parse_e extras;
      }
    in
      (* this package doesn't either have version or name *)
      try 
        Some(exec ()) 
      with Not_found -> 
        None 

  let parse chn = 
    let ch = 
      start_from_channel chn
    in

    let src = 
      match parse_paragraph ch with 
        | Some par ->
            begin
              match parse_source_fields par with
                | Some src -> 
                    src
                | None ->
                    failwith 
                      "Malformed source package"
            end
        | None ->
            failwith 
              "No source package"
    in

    let binaries = 
      parse_822_iter 
        (parse_binary_fields []) 
        (fun s -> s)
        ch
    in
      src, binaries

  let filename =
    Filename.concat "debian" "control"

  let default () = 
    with_fn
      filename
      parse
end

module Changelog = DFChangelog

