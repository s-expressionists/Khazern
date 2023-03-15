(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser simple-type-spec-parser ()
  (lambda (tokens)
    (if (and (not (null tokens))
             (member (car tokens) '(fixnum float t nil)))
        (values t nil
                (car tokens)
                (cdr tokens))
        (values nil nil nil tokens))))

(define-parser destructured-type-spec-parser ()
  (consecutive #'identity
               (keyword :of-type)
               'anything))

(define-parser type-spec-parser ()
  (alternative 'simple-type-spec-parser 'destructured-type-spec-parser))

(define-parser optional-type-spec ()
  (optional t 'type-spec-parser))
