#lang plai

;Problem 1
;Solved by myself
;time taken less than 2 mins
;[contract] dollar -> won, number -> number
;[purpose] convert dollar to won
;[tests] (test (dollar->won 1) 1324)
;        (test (dollar->won 5) 6620)
(define (dollar->won dollar) 
  (* dollar 1324))
(test (dollar->won 1) 1324)
(test (dollar->won 5) 6620)


;Problem 2
;Solved by myself.
;time taken around 5 mins
;[contract] max-of-three-integers, 3 numbers -> 1 number
;[purpose] find the largest number out of 3 integer numbers
;[tests] (test (max-of-three-integers 1 2 3) 3)
;(test (max-of-three-integers 50 20 10) 50)
;(test (max-of-three-integers 2 7 4) 7)

(define (max-of-three-integers num1 num2 num3)
  (cond
    [(>= num1 num2) (cond
                      [(>= num1 num3) num1]
                      [else num3])]
    [else           (cond
                      [(>= num2 num3) num2]
                      [else num3])]))

(test (max-of-three-integers 1 2 3) 3)
(test (max-of-three-integers 50 20 10) 50)
(test (max-of-three-integers 2 7 4) 7)

;Problem 3
;Solved by myself
;time taken around 3 mins
;[contract] volume-cuboid 3 numbers -> number
;[purpose] find the volume of a cuboid using 3 given numbers
;[tests] (test (volume-cuboid 1 5 2) 10)
;        (test (volume-cuboid 5 10 20) 1000)

(define (volume-cuboid length breadth height)
  (* length breadth height))

(test (volume-cuboid 1 5 2) 10)
(test (volume-cuboid 5 10 20) 1000)



;Problem 4
;Solved by having a look into racket docs: https://docs.racket-lang.org/reference/generic-numbers.html
;time taken around 10 mins
;[contract] gcd 2 integer value -> gcd, 2 number -> 1 number
;[purpose] find the gcd of 2 integer values
;[tests] (test 
;        (test 
;        (test 

(define (myGcd num1 num2)
    (cond
      [(> num2 num1) (myGcd num2 num1)]
      [(= (modulo num2 num1) 0) num1]
      [else (myGcd num1 (- num2 1))]))
      


(test (myGcd 8 10) 2)
(test (myGcd 10 8) 2)




;Problem 
;Solved by 
;time taken around 
;[contract] 
;[purpose] 
;[tests] (test 
;        (test 
;        (test 









;Problem 
;Solved by 
;time taken around 
;[contract] 
;[purpose] 
;[tests] (test 
;        (test 
;        (test 










;Problem 
;Solved by 
;time taken around 
;[contract] 
;[purpose] 
;[tests] (test 
;        (test 
;        (test 