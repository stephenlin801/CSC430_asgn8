structure Tests = 
struct 
(* Make sure all tests are in here *)

(* Import everything from ASGN8 *)
open ASGN8

(* Value equality helper function to bypass the 'real' equality restriction *)
    fun checkValEqual (NumV a, NumV b) = Real.== (a, b)
      | checkValEqual (StrV a, StrV b) = (a = b)
      | checkValEqual (BoolV a, BoolV b) = (a = b)
      | checkValEqual _ = false (* Handles mismatched types or PrimV/CloV *)

    
    fun assert (msg, true) = print ("PASS: " ^ msg ^ "\n")
      | assert (msg, false) = raise Fail ("FAIL: " ^ msg)

val _ = print "Running tests...\n"

val _ = assert ("NumC evaluation", 
                 checkValEqual (interp (NumC { n = 5.0 }) [], NumV 5.0));

val _ = assert ("StrC evaluation", 
                 checkValEqual (interp (StrC { s = "hello" }) [], StrV "hello"));
val _ = assert ("BoolC evaluation", 
                 checkValEqual (interp (IdC { id = "true" }) top_env, BoolV true));


(* Make sure all tests are before 'end' *)
end