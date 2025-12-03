(cl:in-package #:khazern-extension)

(defun values-specifier-p (object)
  (and (consp object)
       (eq (car object) 'values)))

(deftype values-specifier ()
  `(satisfies values-specifier-p))

(defmethod khazern:parse-var-spec ((client extension-client) &key (type-spec t))
  (multiple-value-bind (foundp specifier)
      (khazern:maybe-parse-token :type 'values-specifier)
    (if foundp
        (khazern:with-tokens (cdr specifier)
          (prog (vars)
           next
             (when (khazern:more-tokens-p)
               (push (khazern:parse-var-spec client
                                             :type-spec type-spec)
                     vars)
               (go next))
             (return (if (cdr vars)
                         (khazern::make-values-binding (nreverse vars)
                                                       :type type-spec
                                                       :ignorable t)
                         (car vars)))))
        (call-next-method))))
