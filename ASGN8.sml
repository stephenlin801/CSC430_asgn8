(* This is Assignment 4 recreated in SML *)

(* GROUP: Stephen, Bobby, Tyler, Thomas*)

(* Stephen: I created the datatype definitions for the AST, the reserved list, as well as the top_env containing the lambda
functions representing the primitive types. In addition, I started the interp creating the lookup helper 
function and the pattern matches for NumC, StrC, IdC, and IfC *)

(* 
To compile and run code:
    1. Install SML-NJ from https://www.smlnj.org/dist/working/110.99.9/index.html
    2. Run it according to instuctions on page and your OS
    3. Make sure you are in the folder of the project, and if you type:
        'CM.make "sources.cm";'
        into the terminal then it will compile and run the code with the tests
    4. Additionally, once you do that, you should be able to use the command line to run commands
        and call functions of the program 
    5. Let me (Tyler) know if you have any questions
*)

structure ASGN8 =
struct
(* Make sure all code goes in here so it can be compiled and accessed by testing file *)


val reserved = ["if", "=", "given", "fn", "->", "do"]

(* datatype definitions for the VEBG4 AST *)
datatype ExprC = 
    NumC of { n: real }
    | IdC of { id: string }
    | StrC of { s: string }
    | IfC of { cond: ExprC, thenBody: ExprC, elseBody: ExprC }
    | LamC of { params: string list, body: ExprC}
    | AppC of { f: ExprC, args : ExprC list}

(* Useful short hand constructors to make ExprC's with less writing
    ex:
        numC 3.0
    is faster than
        NumC { n = 3.0 }
    and  
        (ifC (idC "true", strC "hello", numC 5.0))
    is alot faster than
        IfC { cond = IdC { id = "true"},
            thenBody = StrC { s = "hello" },
            elseBody = NumC { n = 5.0 }  })
    !!! These won't work for match patterns, for those you have to do them the long way.
    *)
fun numC n = NumC { n = n }
fun idC id = IdC { id = id }
fun strC s = StrC { s = s }
fun ifC (cond, thenBody, elseBody) = IfC { cond = cond, thenBody = thenBody, elseBody = elseBody }
fun lamC (params, body)  = LamC { params = params, body = body }
fun appC (f, args)  = AppC { f = f, args = args }

datatype Value = 
    NumV of real
    | BoolV of bool
    | StrV of string
    | PrimV of { f: Value list -> Value}
    | CloV of { params: string list, body: ExprC, env: (string * Value) list}

fun primV f = PrimV {f = f}
fun cloV (p : string list, b, e) = CloV { params = p, body = b, env = e }

(* top environment containing primitives *)
val top_env = [
    ("+", PrimV { f = fn [NumV a, NumV b] => NumV (a + b)
                        | _ => raise Fail "VEBG: + called incorrectly" }),
    ("-", PrimV { f = fn [NumV a, NumV b] => NumV (a - b)
                        | _ => raise Fail "VEBG: - called incorrectly" }),
    ("*", PrimV { f = fn [NumV a, NumV b] => NumV (a * b)
                        | _ => raise Fail "VEBG: * called incorrectly" }),
    ("/", PrimV { f = fn [NumV a, NumV b] => if Real.==(b, 0.0)
                                                then raise Fail "VEBG: division by zero"
                                                else NumV (a / b)
                        | _ => raise Fail "VEBG: / called incorrectly" }),
    ("<=", PrimV { f = fn [NumV a, NumV b] => BoolV (a <= b)
                        | _ => raise Fail "VEBG: <= called incorrectly" }),
    ("substring", PrimV { f = fn [StrV s, NumV start, NumV stop] => if (start < 0.0) orelse (stop < 0.0) orelse (stop < start)
                                    then raise Fail "VEBG: substring needs positive start and stop value and start needs to be less than stop"
                                    else StrV (String.substring (s, Real.round start, Real.round (stop - start)))
                        | _ => raise Fail "VEBG: substring called incorrectly" }),
    ("strlen", PrimV { f = fn [StrV s] => NumV (real (String.size s))
                        | _ => raise Fail "VEBG: strlen called incorrectly" }),
    ("equal?", PrimV { f = fn [NumV a, NumV b] => BoolV (Real.==(a, b))
                        | [StrV a, StrV b] => BoolV (a = b)
                        | [BoolV a, BoolV b] => BoolV (a = b)
                        | [_, _] => BoolV false
                        | _ => raise Fail "VEBG: equal? called incorrectly" }),
    ("true", BoolV true),
    ("false", BoolV false),
    ("error", PrimV { f = fn [StrV s] => raise Fail s
                        | _ => raise Fail "VEBG: error called incorrectly" })
]

(* Whether the target value is in the list *)
fun contains target [] = false
  | contains target (x::xs) = (target = x) orelse contains target xs;

(* Whether a given list has any duplicates: Use in Parser to check that a LamC has no duplicate params *)
fun hasDuplicates [] = false
  | hasDuplicates (x::xs) = (contains x xs) orelse hasDuplicates xs

(* lookup is a recursive function that checks through a list searching
for the proper value associated with the given id *)
fun lookup (id : string) [] = raise Fail "VEBG: name not found"
    | lookup id ((name, value)::rest) = 
        if id = name
        then value
        else lookup id rest

(* Add a given id-value binding list to the env (returning the new env) *)
fun add_to_env [] [] env = env
    | add_to_env (id::id_r) (value::value_r) env = add_to_env id_r value_r ((id, value)::env)

(* the interp function acts like a large match statement with each
case representing a variation of interp and what the output would be given the env *)
fun interp env (NumC { n = n }) = NumV n
    | interp env (StrC { s = s }) = StrV s
    | interp env (IdC { id = id }) = lookup id env
    | interp env (IfC { cond = cond, thenBody = thenBody, elseBody = elseBody }) = (case interp env cond of
                                                    BoolV true => interp env thenBody
                                                    | BoolV false => interp env elseBody
                                                    | _ => raise Fail "VEBG: interp, need boolean for if")
    | interp env (LamC { params = params, body = body }) = cloV (params, body, env)
    | interp env (AppC { f = f, args = args}) = (case interp env f of
                                                (CloV {params, body, env}) => interp (add_to_env params 
                                                                                        (List.map (interp env) args)
                                                                                        env) 
                                                                                    body
                                                | (PrimV {f = f}) => f (List.map (interp env) args))






(*

interp logic is based off the racket code below if it helps

(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "VEBG: name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                                   [(symbol=? for name) val]
                                   [else (lookup for r)])]))

;; interp takes an ExprC and an Env and returns the Value that
;; ExprC evaluates to using the Env to apply Values to variable names
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(numC n) n]
    [(idC a) (lookup a env)]
    [(strC s) s]
    [(lamC p b) (cloV p b env)]
    [(appC f a) (let ([fi (interp f env)]
                      [ai (map (lambda ([arg : ExprC]) (interp arg env)) a)])
                  (cond
                    [(cloV? fi)
                     (if (not (= (length (cloV-params fi)) (length ai)))
                         (error 'interp "VEBG: wrong arity")
                         (interp (cloV-body fi) (append (map Binding (cloV-params fi) ai) (cloV-env fi))))]
                    [(primV? fi)
                     ((primV-f fi) ai)]
                    [else (error 'interp "VEBG: tried to apply non-function")]))]
    [(ifC a b c) (let ([result (interp a env)])
                   (cond
                     [(not (boolean? result))
                      (error 'interp "VEBG: if conditional not boolean")]
                     [result (interp b env)]
                     [else (interp c env)]))]))
*)

(* All code should be before the 'end' *)
end