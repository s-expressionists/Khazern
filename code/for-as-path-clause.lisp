(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PATH

(defclass for-as-path (for-as-subclause)
  ())

(defmethod map-variables (function (clause for-as-path))
  (map-variables function (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(defparameter *current-path* nil)

(define-parser for-as-path-intro ()
  (consecutive (lambda (var-spec type-spec name)
                 (setf *current-path*
                       (gethash (symbol-name name) (parser-table-paths *parser-table*)))
                 (cl:list name var-spec type-spec))
               'd-var-spec
               'optional-type-spec
               (keyword :being)
               (keyword :each :the)
               'terminal
               (typep '(satisfies loop-path-p))))

(defun loop-path-preposition-p (name)
  (and *current-path*
       (gethash (symbol-name name) (path-prepositions *current-path*))
       t))

(define-parser for-as-path-preposition ()
  (consecutive (lambda (name expression)
                 (cl:list (gethash (symbol-name name) (path-prepositions *current-path*))
                          expression))
               (typep '(satisfies loop-path-preposition-p))
               'anything))

(define-parser for-as-path-parser (:for-as-subclause)
  (lambda (tokens)
    (let ((*current-path* nil))
      (funcall (consecutive (lambda (lead prepositions)
                              (apply (path-function *current-path*)
                                     (first lead)
                                     (second lead)
                                     (third lead)
                                     (path-data *current-path*)
                                     prepositions))
                            'for-as-path-intro
                            (repeat* (lambda (&rest prepositions)
                                       (apply #'append prepositions))
                                     'for-as-path-preposition))
               tokens))))
