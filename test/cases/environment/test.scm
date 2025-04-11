;; Basic environment operations
(define x 5)
x
(define y (+ x 3))
y

;; Shadowing
(define x 10)
x
(let ((x 15)) x)
x

;; Nested environments
(define (outer x)
  (define (inner y)
    (+ x y))
  (inner 3))
(outer 5)

;; Let bindings
(let ((a 1) (b 2))
  (+ a b))

;; Let* bindings
(let* ((a 1)
       (b (+ a 2)))
  (+ a b))

;; Recursive definitions
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 5) 