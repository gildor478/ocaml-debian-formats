
open DFUtils
open ExtLib

let parse ch = 
  let rec parse () = 
    try 
      match IO.read_line ch with 
        | RE bol space* "#" ->
            parse ()
        | RE bol space* "$" ->
            parse ()
        | RE bol space* (_* as str) ->
            begin
              let rec cont_line str = 
                if String.ends_with str "\\" then
                  begin
                    (String.rchop str) ^
                    (try 
                       cont_line (IO.read_line ch)
                     with IO.No_more_input ->
                       "")
                  end
                else
                  str
              in
              let full_line =
                cont_line str 
              in
                full_line :: parse ()
            end
        | _ -> 
            assert false
    with IO.No_more_input ->
      []
  in
    parse ()
