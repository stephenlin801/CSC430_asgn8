structure Tests = 
struct 
(* Make sure all tests are in here *)

(* Import everything from ASGN8 *)
open ASGN8

fun checkTokenEqual (LParen, LParen) = true
  | checkTokenEqual (RParen, RParen) = true
  | checkTokenEqual (TokNum n1, TokNum n2) = Real.== (n1, n2)
  | checkTokenEqual (TokStr s1, TokStr s2) = (s1 = s2)
  | checkTokenEqual (TokId id1, TokId id2) = (id1 = id2)
  | checkTokenEqual _ = false;

fun checkAllTokensEqual ([], []) = true
    | checkAllTokensEqual ((t1::t1s), (t2::t2s)) = checkTokenEqual (t1, t2) andalso checkAllTokensEqual (t1s, t2s)

(* ExprC equality helper function *)
fun checkExprEqual (NumC {n=a}, NumC {n=b}) = Real.abs (a - b) < 0.00001
    | checkExprEqual (StrC {s=a}, StrC{s=b}) = (a = b)
    | checkExprEqual (IdC {id=a}, IdC {id=b}) = (a = b)
    | checkExprEqual (IfC {cond=c1, thenBody=t1, elseBody=e1}, IfC {cond=c2, thenBody=t2, elseBody=e2}) =
                     checkExprEqual (c1, c2)
                     andalso checkExprEqual (t1, t2)
                     andalso checkExprEqual (e1, e2)
    | checkExprEqual (LamC {params=p1, body=b1}, LamC {params=p2, body=b2}) =
                      (p1 = p2)
                      andalso checkExprEqual (b1, b2)
    | checkExprEqual (AppC {f=f1, args=a1}, AppC {f=f2, args=a2}) =
                      checkExprEqual (f1, f2)
                      andalso List.all (fn (x,y) => checkExprEqual (x, y)) (ListPair.zip (a1, a2))
    | checkExprEqual _ = false


(* Value equality helper function to bypass the 'real' equality restriction *)
    fun checkValEqual (NumV a, NumV b) = Real.abs (a - b) < 0.00001
      | checkValEqual (StrV a, StrV b) = (a = b)
      | checkValEqual (BoolV a, BoolV b) = (a = b)
      | checkValEqual _ = false (* Handles mismatched types or PrimV/CloV *)

    
    fun assert (msg, true) = print ("PASS: " ^ msg ^ "\n")
      | assert (msg, false) = raise Fail ("FAIL: " ^ msg)

(* TODO: Add a way for checking for errors *)

val _ = print "Running tests...\n"

val _ = print "--- Lexer tests --- \n"

val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "3", [TokNum 3.0]))
val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "305", [TokNum 305.0]))
val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "3.05", [TokNum 3.05]))
val _ = assert("Number lexing",
                checkAllTokensEqual (lex_string "-3", [TokNum ~3.0]))
val _ = assert("String lexing",
                checkAllTokensEqual (lex_string "\"hello world\"", [TokStr "hello world"]))
val _ = assert("Paren lexing",
                checkAllTokensEqual (lex_string "()())(", [LParen, RParen, LParen, RParen, RParen, LParen]))
val _ = assert("Id lexing",
                checkAllTokensEqual (lex_string "x y z", [TokId "x", TokId "y", TokId "z"]))
val _ = assert("Complex lexing",
                checkAllTokensEqual (lex_string "(x (y \"hi\"  3) () )", 
                    [LParen, TokId "x", LParen, TokId "y", TokStr "hi", TokNum 3.0, RParen, LParen, RParen, RParen]))
(* TODO: Add more lexer tests *)


val _ = print "--- Parser tests ---\n"

val _ = assert ("Parse into NumC",
                 checkExprEqual (parse "3", (NumC { n = 3.0})))
val _ = assert ("Parse into StrC",
                 checkExprEqual (parse "\"hello\"", (StrC { s = "hello"})))
val _ = assert ("Parse into IdC",
                 checkExprEqual (parse "x", (IdC { id = "x" })))
val _ = assert ("Parse into AppC",
                 checkExprEqual (parse "(+ 1 2)", (AppC { f = (IdC { id = "+" }), args = [NumC { n = 1.0 }, NumC { n = 2.0 }] })))


val _ = print "--- Interpreter tests ---\n"

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
val _ = assert ("AppC evaluation - CloV", 
                 checkValEqual (interp top_env (appC (lamC (["x"], appC (idC "*", [idC "x", idC "x"])), [(numC 10.0)])), NumV 100.0)); 
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


val _ = print "--- Serialize tests ---\n"

val _ = assert ("NumV evaluation",
                 serialize (interp top_env (numC 5.1)) = "5.1")
val _ = assert ("StrV evaluation",
                 serialize (interp top_env (strC "hello")) = "\"hello\"")
val _ = assert ("BoolV evaluation",
                 serialize (interp top_env (idC "true")) = "true")
val _ = assert ("CloV evaluation",
                 serialize (interp top_env (lamC (["x"], idC "x"))) = "#<procedure>")
val _ = assert ("PrimV evaluation",
                 serialize (interp top_env (idC "+")) = "#<primop>")

(* Make sure all tests are before 'end' *)
end