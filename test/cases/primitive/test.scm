;; List operations
(car (list 1 2 3))
(cdr (list 1 2 3))
(cons 1 (list 2 3))
(length (list 1 2 3 4 5))
(append (list 1 2) (list 3 4))

;; String operations
(string-append "hello" " " "world")
(string-length "hello")
(substring "hello world" 0 5)

;; Type predicates
(number? 42)
(string? "hello")
(boolean? #t)
(list? (list 1 2 3))
(symbol? 'x)

;; Math functions
(abs -5)
(min 3 4 2 5)
(max 3 4 2 5)
(expt 2 3)
(sqrt 16)

;; List predicates
(null? (list))
(null? (list 1))
(member 3 (list 1 2 3 4)) 