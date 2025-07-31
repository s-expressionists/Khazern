(cl:in-package #:khazern-extension)

(defmethod khazern:preposition-names :around
    ((client extension-client) (instance khazern:being-package-symbols))
  (multiple-value-bind (prepositions required-prepositions usings)
      (call-next-method)
    (values prepositions
            required-prepositions
            (union usings '(:accessibility-type :package)))))

