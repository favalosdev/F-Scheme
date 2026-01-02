(defmacro (unless test conseq alt)
  `(if (not ,test) ,conseq ,alt))

(unless (> 3 4) (+ 2 3) (+ 2 4))
