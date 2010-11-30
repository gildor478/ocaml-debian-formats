
type loc = Lexing.position * Lexing.position
	  
exception Parse_error_822 of string * loc

let with_fn fn f =
  let chn = 
    open_in fn
  in
    try 
      let res = 
        f (IO.input_channel chn)
      in
        close_in chn;
        res
    with e ->
      close_in chn;
      raise e
