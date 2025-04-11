;; Basic literals
42
3.14
"hello"
#t
#f

;; Lists
(list 1 2 3)
(list "a" "b" "c")
(list #t #f #t)

;; Nested lists
(list (list 1 2) (list 3 4))
(list (list "a" "b") (list "c" "d"))

;; Complex expressions
(define x 5)
(define y (+ x 3))
(define (square n) (* n n))
(square y)

;; Lambda expressions
((lambda (x) (+ x 1)) 5)
((lambda (x y) (* x y)) 3 4)

;; Let expressions
(let ((x 1) (y 2)) (+ x y))
(let ((x 5)) (let ((y 6)) (* x y))) 