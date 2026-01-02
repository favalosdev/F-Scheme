(defmacro (dumb-if test conseq alt)
  `(if ,test ,conseq ,alt))

(dumb-if (> 3 4) 'yes 'no)
