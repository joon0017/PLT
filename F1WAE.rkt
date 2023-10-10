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
  [with (name symbol?) (name-exp F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?) (arg F1WAE?)]
  )



;parser
;; parse : sexp -> F1WAE
;; to convert s-expressions into F1WAE's
(define (parse sexp)
	(match sexp
          [(? number?)   (num sexp)]
          [(list '+ l r) (add (parse l) (parse r))]
          [(list '- l r) (sub (parse l) (parse r))]
          [(list 'with   (list i v) e) (with i (parse v) (parse e))]
          [(? symbol?)   (id sexp)]
          [(list f a)    (app f (parse a))]
          [else          (error 'parse "bad syntax: ~a" sexp)]))

(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]))


;interpreter

;interp: F1WAE list-of-FuncDef -> number
; interp: F1WAE list-of-FuncDef -> number
(define (interp f1wae fundefs)
	(type-case F1WAE f1wae
		[num	(n)		n]
		[add 	(l r)		(+ (interp l fundefs) (interp r fundefs))]
		[sub 	(l r)		(- (interp l fundefs) (interp r fundefs))]
		[with	(x i b)	(interp (subst b x (interp i fundefs)) fundefs)]
		[id 		(s)		(error 'interp "free identifier")]
		[app     (f a)	
	                              (local
				             [(define a_fundef (lookup-fundef f fundefs))]
				             (interp (subst (fundef-body a_fundef)
								          (fundef-arg-name a_fundef)
							                 (interp a fundefs))
							      fundefs))]))

;lookup-fundef: symbol list-of-FunDef -> FunDef
(define (lookup-fundef name fundefs)
  (cond
    [(empty? fundefs) (error 'lookup-fundef "unknown function")]
    [else (if (symbol=? name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef name (rest fundefs)))]))

; [contract] subst: F1WAE symbol number -> F1WAE
(define (subst f1wae idtf val)
	(type-case F1WAE f1wae
		[num	(n)		f1wae]
		[add	(l r) 		(add (subst l idtf val) (subst r idtf val))]
		[sub		(l r)	 	(sub (subst l idtf val) (subst r idtf val))]
		[with	(i v e) 	(with i (subst v idtf val) (if (symbol=? i idtf) e
									(subst e idtf val)))]
		[id		(s) 		(if (symbol=? s idtf) (num val) f1wae)]
		[app	(f a)		(app f	(subst a idtf val))]))


(test (interp (app 'f (num 1)) (list (fundef 'f 'x (add (id 'x) (num 3))))) 4)

(test (interp (app 'f (num 20))
              (list (fundef 'f 'x (sub (num 20)
                                       (app 'twice (id 'x))))
                    (fundef 'twice 'y (add (id 'y) (id 'y)))))
      -20)
    
(subst (app 'fn (with 'y (num 10) (add (id 'y) (id 'x)))) 'x 1)

(interp (app 'foo (app 'bar (num 5))) (list (fundef 'foo 'a (add (id 'a) (num 5)))
                                            (fundef 'bar 'b (sub (id 'b) (num 10)))))

Q: Why do we need a lookup function, and how does it work?

A: A lookup function is used to help find the correct function in the list of defined functions. The lookup function scans through the list recursively until it finds the function it was looking for. If it fails, it returns with an error.