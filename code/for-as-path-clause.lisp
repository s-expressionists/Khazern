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

(defparameter *current-path-usings* nil)

(define-parser for-as-path-intro ()
  (consecutive (lambda (var-spec type-spec args)
                 (setf *current-path*
                       (funcall (iterator-path *parser-table* (car args))
                                (car args) var-spec type-spec))
                 (when (cdr args)
                   (unless (path-inclusive-permitted-p *current-path*)
                     (error 'loop-path-non-inclusive :path (car args)))
                   (setf (path-preposition instance
                                           (symbol-lookup (path-preposition-names instance :of)))
                         (second args)
                         (path-inclusive-p instance) t))
                 *current-path*)
               'd-var-spec
               'optional-type-spec
               (keyword :being)
               'terminal
               (alternative (consecutive (lambda (path)
                                           (cl:list path))
                                         (keyword :each :the)
                                         'terminal
                                         (typep '(satisfies loop-path-p)))
                            (consecutive (lambda (expression path)
                                           (cl:list path expression))
                                         'anything
                                         (keyword :and)
                                         (keyword :its :each :his :her)
                                         'terminal
                                         (typep '(satisfies loop-path-p))))))
 
(defun current-path-preposition-p (name)
  (and *current-path*
       (let ((key (symbol-lookup name (path-preposition-names *current-path*))))
         (and key
              (not (member key *current-path-prepositions*))))
       t))

(defun current-path-using-p (name)
  (and *current-path*
       (not (member :using *current-path-prepositions*))
       (let ((key (symbol-lookup name (path-using-names *current-path*))))
         (and key
              (not (member key *current-path-usings*))))
       t))

(define-parser for-as-path-preposition ()
  (consecutive (lambda (name expression)
                 (let ((key (symbol-lookup name (path-preposition-names *current-path*))))
                   (setf (path-preposition *current-path* key) expression)
                   (push key *current-path-prepositions*))
                 nil)
               (typep '(satisfies current-path-preposition-p))
               'anything))

(define-parser for-as-path-using ()
  (consecutive (lambda (&rest args)
                 (push :using *current-path-prepositions*)
                 args)
               (keyword :using)
               (list (lambda (&rest args)
                       args)
                     (repeat+ (lambda (&rest args)
                                args)
                              (consecutive (lambda (name expression)
                                             (let ((key (symbol-lookup name (path-using-names *current-path*))))
                                               (setf (path-using *current-path* key) expression)
                                               (push key *current-path-usings*)))
                                           (typep '(satisfies current-path-using-p))
                                           'anything)))))

(define-parser for-as-path-parser (:for-as-subclause)
  (lambda (tokens)
    (let ((*current-path* nil)
          (*current-path-prepositions* nil)
          (*current-path-usings* nil))
      (funcall (consecutive (lambda (path preps)
                              path)
                            'for-as-path-intro
                            (repeat* (lambda (&rest args)
                                       args)
                                     (alternative 'for-as-path-preposition
                                                  'for-as-path-using)))
               tokens))))
