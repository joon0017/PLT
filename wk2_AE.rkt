#lang plai
;Arithmatic Expression
(define-type AE 
  [num (n number?)]
  [add (lft AE?) (rgt AE?)]
  [sub (lft AE?) (rgt AE?)])

;(+ 1 2)
(add (num 1) (num 2))