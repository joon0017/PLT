#|

HW2: Implementing F1WAE with Deferred Substitution and Desugaring
21900699 Seong Joon Cho

Task 1
Solved by myself: Yes
Time taken: 2 hours
[Contract] interp, f1wae fundefs ds -> num | add | sub | with | id | app
[Purpose] update f1wae interpreter so that F1WAE can use deferred substitution
tests:
    (test (interp (parse '{f 1}) (list (parse-fd '{deffun (f x) {+ x 3}})) (mtSub))4)
    (test (interp (parse '{f variable}) (list (parse-fd '{deffun (f x) {+ x 10}})) (aSub 'variable 10 (mtSub)))20)
    (test (interp (parse '(function1 (function2 myVariable)))
              (list (parse-fd '(deffun (function1 x) (+ x 5)))
                    (parse-fd '(deffun (function2 x) (+ x 20))))
              (aSub 'myVariable 3 (mtSub))) 28)

Task 2
Solved by myself: No
Refered website(s): https://docs.racket-lang.org/guide/for.html
                    https://www.reddit.com/r/Racket/comments/9f1jwu/how_do_i_set_a_variable_to_be_a_value/?rdt=59721
                    https://docs.racket-lang.org/guide/set_.html
Time taken 7 hours
[Contract] define-type F1WAE -> add [mult], desugar f1wae : num, num -> num
[Purpose] Update the language to support multiplication by applying syntactic sugaring and desugaring
tests:
    (test (interp (parse '(* 2 3)) '() (mtSub)) 6)
    (test (interp (parse '(* 8 (+ 2 3))) '() (mtSub)) 40)


There are test cases for each tasks at the end of this file, along with Task 3.

|#

#lang plai

;types
(define-type FunDef
  [fundef (fun-name symbol?) ;;<- function name
          (arg-name symbol?) ;;<- argument name
          (body F1WAE?)])    ;;<- body part


(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [mult (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (name-exp F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?) (arg F1WAE?)]
  )

;deferred substitution type
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])

;parser
;; parse : sexp -> F1WAE
;; to convert s-expressions into F1WAE's
(define (parse sexp)
	(match sexp
          [(? number?)   (num sexp)]
          [(list '+ l r) (add (parse l) (parse r))]
          [(list '- l r) (sub (parse l) (parse r))]
          [(list '* l r) (mult (parse l) (parse r))]
          [(list 'with   (list i v) e) (with i (parse v) (parse e))]
          [(? symbol?)   (id sexp)]
          [(list f a)    (app f (parse a))]
          [else          (error 'parse "bad syntax: ~a" sexp)]))

(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]))

;interpreter

; interp: F1WAE list-of-FuncDef -> number
(define (interp f1wae fundefs ds)
  (type-case F1WAE f1wae
    [num (n) n]
    [add (l r) (+ (interp l fundefs ds) (interp r fundefs ds))]
    [sub (l r) (- (interp l fundefs ds) (interp r fundefs ds))]
    [mult (l r) (interp (desugar (mult l r)) fundefs ds)]
    [with (x i v) (interp (subst v x (interp i fundefs ds)) fundefs ds)]
    [id (s) (lookup s ds)]
    [app (f a)
         (local
           [(define a-fundef (lookup-fundef f fundefs))]
           (interp (fundef-body a-fundef)
                   fundefs
                   (aSub (fundef-arg-name a-fundef)
                         (interp a fundefs ds)
                         (mtSub))
                   ))]))

;desugar function for multiplication
(define (desugar f1wae)
  (type-case F1WAE f1wae
    [mult (lhs rhs)
          (let ((result (num 0)))  
            (for ([i (in-range (interp lhs '() (mtSub)))])
              (set! result (add result (desugar rhs))))
            result)]
    [else f1wae]))

;lookup-fundef: symbol list-of-FunDef -> FunDef
(define (lookup-fundef name fundefs)
  (cond
    [(empty? fundefs) (error 'lookup-fundef "unknown function")]
    [else (if (symbol=? name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef name (rest fundefs)))]))

;lookup: symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "free identifier")]
    [aSub (i v saved) (if (symbol=? i name)
                          v
                          (lookup name saved))]))

;subst: F1WAE symbol number -> F1WAE
(define (subst f1wae idtf val)
  (type-case F1WAE f1wae
    [num	(n)		f1wae]
    [add	(l r) 		(add (subst l idtf val) (subst r idtf val))]
    [sub	(l r)	 	(sub (subst l idtf val) (subst r idtf val))]
    [mult	(l r)		(mult (subst l idtf val) (subst r idtf val))]
    [with	(i v e) 	(with i (subst v idtf val) (if (symbol=? i idtf) e
                                                               (subst e idtf val)))]
    [id		(s) 		(if (symbol=? s idtf) (num val) f1wae)]
    [app	(f a)		(app f	(subst a idtf val))]))


#|

Test cases for HW2: Implementing F1WAE with Deferred Substitution and Desugaring

|#

; test case for task 1
(test (interp (parse '{f 1}) (list (parse-fd '{deffun (f x) {+ x 3}})) (mtSub))4)
(test (interp (parse '{f variable}) (list (parse-fd '{deffun (f x) {+ x 10}})) (aSub 'variable 10 (mtSub)))20)
(test (interp (parse '(function1 (function2 myVariable)))
              (list (parse-fd '(deffun (function1 x) (+ x 5)))
                    (parse-fd '(deffun (function2 x) (+ x 20))))
              (aSub 'myVariable 3 (mtSub))) 28)

; test cases for task 2
(test (interp (parse '(* 2 3)) '() (mtSub)) 6)
(test (interp (parse '(* 8 (+ 2 3))) '() (mtSub)) 40)

#|

Task 3
(1 Point) Q1. Which scope is supported for a free identifier in a function call in your implementation, static scope, dynamic scope or both?
-> My implementation supports static scopes. This means the values inside a function call is determined when the function is defined, not when it is called.
(1 Point) Put the three test cases that show your answer for Q1 is correct.
|#

; This test case shows my implementation supports static scope.
; The inner x will have a value of 2, with the outer x having a value of 5. When the x is changed inside the code, it will not affect the outer x.
(test (interp (with 'x (num 5) {add (with 'x (num 2) (id 'x)) (id 'x)})  '() (mtSub)  ) 7)


;this test case shows my implementation supports static scope.
(test (interp {add (id 'x) (with 'x (num 5) {add (with 'x (num 2) (id 'x)) (id 'x)}) } '() (aSub 'x 10 (mtSub))  ) 17)

;this test case shows my implementation does not support dynamic scope. this should result in an error, with the free identifier error message.
(test/exn (interp{add (id 'x) (with 'y (num 5) {add (with 'x (num 2) (id 'x)) (id 'y)})} '() (mtSub)) "free identifier")