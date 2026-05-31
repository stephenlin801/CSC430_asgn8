structure Tests = 
struct 
(* Make sure all tests are in here *)

(* Import everything from ASGN8 *)
open ASGN8

fun checkTokenEqual (LPrend, LPrend) = true
  | checkTokenEqual (RPrend, RPrend) = true
  | checkTokenEqual (TokNum n1, TokNum n2) = Real.== (n1, n2)
  | checkTokenEqual (TokStr s1, TokStr s2) = (s1 = s2)
  | checkTokenEqual (TokId id1, TokId id2) = (id1 = id2)
  | checkTokenEqual _ = false;

fun checkAllTokensEqual ([], []) = true
    | checkAllTokensEqual ((t1::t1s), (t2::t2s)) = checkTokenEqual (t1, t2) andalso checkAllTokensEqual (t1s, t2s)

(* Value equality helper function to bypass the 'real' equality restriction *)
    fun checkValEqual (NumV a, NumV b) = Real.abs (a - b) < 0.00001
      | checkValEqual (StrV a, StrV b) = (a = b)
      | checkValEqual (BoolV a, BoolV b) = (a = b)
      | checkValEqual _ = false (* Handles mismatched types or PrimV/CloV *)

    
    fun assert (msg, true) = print ("PASS: " ^ msg ^ "\n")
      | assert (msg, false) = raise Fail ("FAIL: " ^ msg)
(* TODO: Add a way for checking for errors *)
val _ = print "Running tests...\n"

val _ = print "Lexer tests"

val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "3", [TokNum 3.0]))
val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "305", [TokNum 305.0]))
val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "3.05", [TokNum 3.05]))
val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "-3", [TokNum ~3.0]))
(* TODO: Add mroe lexer tests *)

val _ = print "Interpreter tests\n"

val _ = assert ("NumC evaluation", 
                 checkValEqual (interp top_env (NumC { n = 5.0 }), NumV 5.0));

val _ = assert ("StrC evaluation", 
                 checkValEqual (interp top_env (StrC { s = "hello" }), StrV "hello"));
val _ = assert ("BoolC evaluation", 
                 checkValEqual (interp top_env (IdC { id = "true" }), BoolV true));
val _ = assert ("IfC evaluation - true branch", 
                 checkValEqual (interp top_env (ifC (idC "true", strC "hello", numC 5.0)), StrV "hello"));

val _ = assert ("IfC evaluation - false branch", 
                 checkValEqual (interp top_env (ifC (idC "false", strC "hello", numC 5.0)), NumV 5.0));
val _ = assert ("AppC evaluation - CloV", 
                 checkValEqual (interp top_env (appC (lamC (["x"], idC "x"), [(numC 1.1)])), NumV 1.1)); 
val _ = assert ("AppC evaluation - +", 
                 checkValEqual (interp top_env (appC ((idC "+"), [(numC 1.1), (numC 2.0)])), NumV 3.1));
val _ = assert ("AppC evaluation - -", 
                 checkValEqual (interp top_env (appC ((idC "-"), [(numC 1.1), (numC 2.0)])), NumV ~0.9));
val _ = assert ("AppC evaluation - *", 
                 checkValEqual (interp top_env (appC ((idC "*"), [(numC 1.1), (numC 2.0)])), NumV 2.2));
val _ = assert ("AppC evaluation - /", 
                 checkValEqual (interp top_env (appC ((idC "/"), [(numC 1.1), (numC 2.0)])), NumV 0.55));
val _ = assert ("AppC evaluation - <=", 
                 checkValEqual (interp top_env (appC ((idC "<="), [(numC 1.1), (numC 2.0)])), BoolV true));
val _ = assert ("AppC evaluation - <=", 
                 checkValEqual (interp top_env (appC ((idC "<="), [(numC 6.1), (numC 2.0)])), BoolV false));
val _ = assert ("AppC evaluation - substring", 
                 checkValEqual (interp top_env (appC ((idC "substring"), [(strC "hello"), (numC 2.0), (numC 5.0)])), StrV "llo"));

val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(numC 2.0), (numC 5.0)])), BoolV false));
val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(numC 2.0), (numC 2.0)])), BoolV true));
val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(strC "hi"), (strC "hi")])), BoolV true));
val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(strC "hi"), (strC "hello")])), BoolV false));
val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(idC "true"), (idC "true")])), BoolV true));
val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(idC "false"), (idC "true")])), BoolV false));
val _ = assert ("AppC evaluation - equal?", 
                 checkValEqual (interp top_env (appC ((idC "equal?"), [(idC "false"), (idC "false")])), BoolV true));

(* Make sure all tests are before 'end' *)
end