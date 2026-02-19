(define-syntax for
  (syntax-rules (in)
    ((for var in list body ...)
     (map (lambda (var) body ...) list))))