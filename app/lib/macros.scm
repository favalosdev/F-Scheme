(defmacro (unless test conseq)
  `(if (not (not (not (not ,test)))) ,conseq))

(unless (< 3 4) 'no)
