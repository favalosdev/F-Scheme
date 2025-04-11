;; Type errors
(+ "hello" 1)
(* #t 5)
(car 42)

;; Undefined variables
undefined-variable
(+ x y)

;; Wrong number of arguments
(+ 1 2 3 4)
(car (list 1 2) (list 3 4))

;; Division by zero
(/ 5 0)

;; Invalid syntax
(if #t)
(define)

;; Invalid list operations
(car (list))
(cdr (list)) 