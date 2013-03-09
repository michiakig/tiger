structure Parse =
struct
local fun makeReaderFromString s =
          let val pos = ref 0
          in
              fn _:int => if String.size s <= !pos
                          then ""
                          else let val s' = String.substring (s,!pos,1)
                               in (pos := !pos + 1; s')
                               end
          end
      fun do_it lexer =
	  let val t = lexer()
	  in print t; print "\n";
	     if substring(t,0,3)="EOF" then () else do_it lexer
	  end
in
fun parseFile filename =
    let val file = TextIO.openIn filename
	fun get _ = TextIO.input file
	val lexer = Mlex.makeLexer get
    in do_it lexer;
       TextIO.closeIn file
    end
fun parseString str =
    let val lexer = Mlex.makeLexer (makeReaderFromString str)
    in
        do_it lexer
    end
end
end

