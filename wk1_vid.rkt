#lang plai

(define-type Animal
  (bear (num_child number?)
        (color string?))
  (giraffe (height number?)
           (name string?)
           (len_neck number?))
  (snake (length number?)
         (poison boolean?)))
(define myBear(bear 2 "asdf"))
(define myGiraffe(giraffe 200 "sj" 2))
(define mySnake(snake 3 #t))
myBear
myGiraffe

(define (getnumber a)
  (type-case Animal a
    [bear (n c) n]
    [giraffe (h n l_n) (list h l_n)]
    [snake (l p?) l]))

(getnumber mySnake)