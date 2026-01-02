(defmacro (unless test conseq alt)
  `(if ,test ,conseq ,alt))

(unless (> 3 4) (+ 2 3) (+ 2 4))
