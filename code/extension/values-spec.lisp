(cl:in-package #:khazern-extension)

(defmethod khazern:parse-var-spec ((client extension-client) &key (type-spec t))
  (multiple-value-bind (foundp specifier)
      (khazern:maybe-parse-token :type 'values-specifier)
    (if foundp
        (khazern::make-values-binding (khazern:with-tokens (cdr specifier)
                                        (loop while (khazern:more-tokens-p)
                                              collect (khazern:parse-var-spec client :type-spec type-spec)))
                                      :type type-spec
                                      :ignorable t)
        (call-next-method))))
