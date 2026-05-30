(* This is Assignment 4 recreated in SML *)

(* GROUP: Stephen, Bobby, Tyler, Thomas*)

(* datatype definitions for AST *)

val reserved = ["if", "=", "given", "fn", "->", "do"]

datatype ExprC = 
    NumC of { n: real }
    | IdC of { i: string }
    | StrC of { s: string }
    | IfC of { a: ExprC, b: ExprC, c: ExprC }
    | LamC of { p: string list}
    | AppC of { f: ExprC, a : ExprC list}

datatype Value = 
    NumV of real
    | BoolV of bool
    | StrV of string
    | PrimV of { f: Value list -> Value}
    | CloV of { params: string list, body: ExprC, env: (string * PrimV) list}

(* top environment is currently incomplete, I will finish it before Saturday *)
val top_env = [
    ("+", PrimV {}),
    ("-", PrimV {}),
    ("*", PrimV {}),
    ("/", PrimV {}),
    ("<=", PrimV {}),
    ("substring", PrimV {}),
    ("strlen", PrimV {}),
    ("equal?", PrimV {}),
    ("true", BoolV true),
    ("false", BoolV false),
    ("error", PrimV {})
]

(*

(define (check-execute-binop [f : (Real Real -> Real)][args : (Listof Value)]) : Value
  (cond
    [(not (= (length args) 2))
     (error 'interp "VEBG: wrong arity")]
    [(not (and (real? (first args)) (real? (second args))))
     (error 'interp "VEBG: arguments are not real number")]
    [else (f (cast (first args) Real) (cast (second args) Real))]))


(define top-env (list
                 (Binding '+ (primV (lambda (args) (check-execute-binop + args))))
                 (Binding '-  (primV (lambda (args) (check-execute-binop - args))))
                 (Binding '* (primV (lambda (args) (check-execute-binop * args))))
                 (Binding '/ (primV (lambda (args) (cond
                                                     [(not (= (length args) 2))
                                                      (error 'interp "VEBG: wrong arity")]
                                                     [(not (and (real? (first args)) (real? (second args))))
                                                      (error 'interp "VEBG: arguments are not real number")]
                                                     [(= (cast (second args) Real) 0)
                                                      (error 'interp "VEBG: cannot divide by 0")]
                                                     [else (/ (cast (first args) Real) (cast (second args) Real))]))))
                 (Binding '<= (primV (lambda (args) (cond
                                                      [(not (= (length args) 2))
                                                       (error 'interp "VEBG: wrong arity")]
                                                      [(not (and (real? (first args)) (real? (second args))))
                                                       (error 'interp "VEBG: arguments are not real number")]
                                                      [else (<= (cast (first args) Real)
                                                                (cast (second args) Real))]))))
                 (Binding 'substring (primV (lambda (args)
                                              (cond
                                                [(not (= (length args) 3))
                                                 (error 'interp "VEBG: wrong arity")]
                                                [(not (string? (first args)))
                                                 (error 'interp "VEBG: first argument is not string")]
                                                [(not (and (exact-nonnegative-integer? (second args))
                                                           (exact-nonnegative-integer? (third args))))
                                                 (error 'interp
                                                        "VEBG: second or third arguments are not natural numbers")]
                                                [else
                                                 (let ([str-len (string-length (cast (first args) String))]
                                                       [str (cast (first args) String)]
                                                       [start (cast (second args) Integer)]
                                                       [stop (cast (third args) Integer)])
                                                   (cond
                                                     [(> stop str-len)
                                                      (error 'interp "VEBG: index out of range")]
                                                     [(> start stop)
                                                      (error 'interp "VEBG: start greater than stop")]
                                                     [else (substring str start stop)]))]))))
                 (Binding 'strlen (primV (lambda (args) (cond
                                                          [(not (= (length args) 1))
                                                           (error 'interp "VEBG: wrong arity")]
                                                          [(not (string? (first args)))
                                                           (error 'interp "VEBG: argument is not string")]
                                                          [else (string-length (cast (first args) String))]))))
                 (Binding 'equal? (primV (lambda (args) (cond
                                                          [(not (= (length args) 2))
                                                           (error 'interp "VEBG: wrong arity")]
                                                          [(or (cloV? (first args))
                                                               (cloV? (second args))
                                                               (primV? (first args))
                                                               (primV? (second args))) false]
                                                          [else (equal? (first args) (second args))]))))
                 (Binding 'true true)
                 (Binding 'false false)
                 (Binding 'error (primV (lambda (args)
                                          (cond
                                            [(not (= (length args) 1))
                                             (error 'interp "VEBG: wrong arity")]
                                            [else (error 'user-error
                                                         "VEBG user-error: ~a"
                                                         (serialize (first args)))]))))))

*)