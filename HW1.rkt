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
;[contract] max-of-three-integers, number number number -> number
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
;[contract] volume-cuboid, number number number -> number
;[purpose] find the volume of a cuboid using 3 given numbers
;[tests] (test (volume-cuboid 1 5 2) 10)
;        (test (volume-cuboid 5 10 20) 1000)

(define (volume-cuboid length breadth height)
  (* length breadth height))

(test (volume-cuboid 1 5 2) 10)
(test (volume-cuboid 5 10 20) 1000)



;Problem 4
;Solved with the help of this website: https://dyclassroom.com/recursion-algorithm/greatest-common-divisor-gcd
;time taken around 3 mins
;[contract] myGcd, number number -> number
;[purpose] find the gcd of 2 integer values
;[tests] (test (myGcd 15 25) 5)
;        (test (myGcd 10 8) 2)

(define (myGcd num1 num2)
    (cond
      [(= num2 0) num1]
      [else (myGcd num2 (modulo num1 num2))]))

(test (myGcd 15 25) 5)
(test (myGcd 10 8) 2)




;Problem 5
;Solved by myself
;time taken around 7 mins
;[contract] combination, number number -> number
;[purpose] To find the number of combinations with the 2 numbers given
;[tests] (test (combination 10 2)45)
;        (test (combination 5 3)10)

(define (factorial n)
  (cond
    [(<= n 1) 1]
    [else (* n (factorial (- n 1)))]))

(define (combination n k)
  ( / (factorial n) (* (factorial (- n k)) (factorial k))))

(test (combination 10 2)45)
(test (combination 5 3)10)





;Problem 6
;Solved by myself
;time taken around 30 mins
;[contract] a) define-type Vehicle
;           b) vehicle-tax, Vehicle->number
;           c) is-vehicle-safe, Vehicle->string
;
;[purpose]  a) To define a type named Vehicle
;           b) To calculate the tax each vehicle has to pay
;           c) To determine whether a vehicle is safe or not
;[tests]
;        a)
;        myBike, myCar, myAirplane
;
;        b)
;        (test (vehicle-tax myBike) 2)
;        (test (vehicle-tax myAirplane) 14)
;
;        c)
;        (test (is-vehicle-safe myBike) "safe")
;        (test (is-vehicle-safe myCar) "safe")
;        (test (is-vehicle-safe myAirplane) "unsafe")

;a)
(define-type Vehicle
  (Bicycle (wheels number?))
  (Car (wheels number?) (windows number?))
  (Airplane (wheels number?) (windows number?) (engines number?)))

(define myBike(Bicycle 2))
myBike
(define myCar(Car 4 6))
myCar
(define myAirplane(Airplane 4 8 2))
myAirplane

;b)
(define (vehicle-tax v)
  (type-case Vehicle v
    [Bicycle (wheel) wheel]
    [Car (wheel window) (+ wheel window)]
    [Airplane (wheel window engine) (+ wheel window engine)]))

(test (vehicle-tax myBike) 2)
(test (vehicle-tax myAirplane) 14)

;c)
(define (is-vehicle-safe v)
  (type-case Vehicle v ;check each type separately
    [Bicycle (wheel) ;bike
     (cond
       [(< wheel 4) "safe"]
       [else "unsafe"])]
    [Car (wheel window) ;car
      (cond
        [(> wheel 3) (cond
                       [(> window 2) "safe"]
                       [else "unsafe"])
                     ]
        [else "unsafe"])]
    [Airplane (wheel window engine) ;airplane
      (cond
        [(> wheel 2) (cond
                       [(> window 10) (cond
                                        [(> engine 1) "safe"]
                                        [else "unsafe"])
                                      ]
                       [else "unsafe"]
                       )
          ]
        [else "unsafe"])]))


(test (is-vehicle-safe myBike) "safe")
(test (is-vehicle-safe myCar) "safe")
(test (is-vehicle-safe myAirplane) "unsafe")

;Problem 7
;Solved by myself
;time taken around 40 mins
;[contract] name-alphabet, list->list
;[purpose] take a list of alphabets and produce a list of names
;          starting with the corresponding alphabets in the given list
;[tests] (test (name-alphabet '(a b c i j k))'(alice unnamed cherry unnamed jc kate))
;        (test (name-alphabet '(k j i c b a))'(kate jc unnamed cherry unnamed alice unnamed))



(define (name-alphabet lst)
  (cond [(empty? lst) empty]
        
        ; only check for a, c, j, k
        [(equal? (first lst) 'a) (cons 'alice (name-alphabet(rest lst)))]
        [(equal? (first lst) 'c) (cons 'cherry (name-alphabet(rest lst)))]
        [(equal? (first lst) 'j) (cons 'jc (name-alphabet(rest lst)))]
        [(equal? (first lst) 'k) (cons 'kate (name-alphabet(rest lst)))]
        
        [else (append '(unnamed) (name-alphabet(rest lst)))]
   ))

(test (name-alphabet '(a b c i j k))'(alice unnamed cherry unnamed jc kate))
(test (name-alphabet '(k j i c b a x))'(kate jc unnamed cherry unnamed alice unnamed))

;Problem 8
;Solved by myself
;time taken around 5~10 mins
;[contract] update-name, symbol symbol list -> list
;[purpose] Return a list that has all old symbols replaced with new symbols
;[tests] (test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
;        (test (update-name 'alice 'aaron '(sj alice cherry kate)) '(sj aaron cherry kate))


(define (update-name old new lst) ;recursively check whole list, one by one
  (cond [(empty? lst) empty]

        ;check for replacements
        [(equal? (first lst) old) (cons new (update-name old new (rest lst)))]

        ;else return original symbol
        [else (cons (first lst) (update-name old new (rest lst)))]))

(test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
(test (update-name 'alice 'aaron '(sj alice cherry kate)) '(sj aaron cherry kate))