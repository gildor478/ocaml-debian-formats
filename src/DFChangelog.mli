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
