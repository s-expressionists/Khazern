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

(defparameter *current-path-prepositions* nil)

(define-parser for-as-path-intro ()
  (consecutive (lambda (var-spec type-spec name)
                 (setf *current-path*
                       (funcall (gethash (symbol-name name)
                                         (parser-table-paths *parser-table*))
                                name var-spec type-spec)))
               'd-var-spec
               'optional-type-spec
               (keyword :being)
               (keyword :each :the)
               'terminal
               (typep '(satisfies loop-path-p))))

(defun current-path-preposition-p (name)
  (and *current-path*
       (let ((key (path-preposition-key *current-path* name)))
         (and key
              (not (member key *current-path-prepositions*))))
       t))

(define-parser for-as-path-preposition ()
  (consecutive (lambda (name expression)
                 (let ((key (path-preposition-key *current-path* name)))
                   (setf (path-preposition *current-path* key) expression)
                   (push key *current-path-prepositions*))
                 (values))
               (typep '(satisfies current-path-preposition-p))
               'anything))

(define-parser for-as-path-parser (:for-as-subclause)
  (lambda (tokens)
    (let ((*current-path* nil)
          (*current-path-prepositions* nil))
      (funcall (consecutive (lambda (path preps)
                              path)
                            'for-as-path-intro
                            (repeat* (lambda (&rest args)
                                       args)
                                     'for-as-path-preposition))
               tokens))))
