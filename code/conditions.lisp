(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for parsing
;;;
;;; FIXME: Remove condition reporters from the DEFINE-CONDITION forms
;;; and put them in a separate (language-specific) file.  

(define-condition loop-parse-error (parse-error acclimation:condition)
  ((location :accessor location
             :initarg :location
             :initform nil
             :type list)))

(define-condition loop-parse-error-found (loop-parse-error)
  ((%found :reader found
           :initarg :found)))

(define-condition expected-var-spec-but-end (loop-parse-error)
  ())

(define-condition expected-var-spec-but-found (loop-parse-error-found)
  ())

(define-condition expected-token (loop-parse-error)
  ((%expected-type :accessor expected-type
                   :initarg :expected-type
                   :initform nil)
   (%expected-keywords :accessor expected-keywords
                       :initarg :expected-keywords
                       :initform nil
                       :type list)))

(define-condition expected-token-but-found (expected-token loop-parse-error-found)
  ())

(define-condition expected-token-but-end (expected-token)
  ())

(define-condition unexpected-tokens-found (loop-parse-error-found)
  ())

(define-condition too-many-prepositions-from-one-group (loop-parse-error)
  ())

(define-condition conflicting-stepping-directions (loop-parse-error)
  ())

(defun combine-parse-errors (x y)
  (cond ((and (null x) (null y))
         nil)
        ((null x)
         y)
        ((or (null y)
             (< (car (location x)) (car (location y))))
         x)
        ((> (car (location x)) (car (location y)))
         y)
        ((and (cl:typep x 'expected-token)
              (cl:typep y 'expected-token))
         (setf (expected-keywords x) (append (expected-keywords x) (expected-keywords y)))
         (cond ((and (null (expected-type x))
                     (null (expected-type y))))
               ((null (expected-type y)))
               ((or (null (expected-type x))
                    (subtypep (expected-type x) (expected-type y)))
                (setf (expected-type x) (expected-type y)))
               ((subtypep (expected-type y) (expected-type x)))
               (t
                (setf (expected-type x) `(or ,(expected-type x) ,(expected-type y)))))
         x)
        (t
         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for syntactic and semantic analysis
;;;
;;; FIXME: Define English language condition reporters for these
;;;        conditions.

;;; The root of all syntax errors.
(define-condition loop-syntax-error (program-error acclimation:condition)
  ())

;;; This condition is signaled when a NIL is found
;;; in a simple LOOOP
(define-condition non-compound-form (loop-syntax-error) ())

;;; This condition is signaled when a name-clause is found
;;; and the first clause is not a name-clause. 
(define-condition name-clause-not-first (loop-syntax-error) ())

;;; This condition is signaled when the first clause is a name-clause
;;; but there are other name-clauses. 
(define-condition multiple-name-clauses (loop-syntax-error) ())

;;; This condition is signaled when a variable-clause (other than an
;;; initially-clause or a finally-clause) appears after a main-clause
;;; (other than an initially-clause or a finally-clause).  
(define-condition invalid-clause-order (loop-syntax-error) ())

;;; This condition is signaled when there are multiple occurrences of
;;; a variable to be bound by any loop clause.
(define-condition multiple-variable-occurrences (loop-syntax-error)
  ((%bound-variable :initarg :bound-variable :reader bound-variable)))

;;; This condition is signaled when there are overlaps betweeen
;;; iteration and accumulation variables.
(define-condition iteration-accumulation-overlap (loop-syntax-error)
  ((%bound-variable :initarg :bound-variable :reader bound-variable)))

;;; This condition is signaled when there are multiple occurrences of
;;; a variable used in accumulation clauses.
(define-condition multiple-accumulation-occurrences (loop-syntax-error)
  ((%bound-variable :initarg :bound-variable :reader bound-variable)
   (%first-clause :initarg :first-clause :reader first-clause)
   (%second-clause :initarg :second-clause :reader second-clause)))

;;; The root of all semantic errors.
(define-condition loop-semantic-error (program-error acclimation:condition)
  ())

(define-condition loop-path-non-inclusive (loop-semantic-error)
  ((%path :reader path
          :initarg :path)))

;;; Run Time Conditions

(define-condition loop-runtime-error (error acclimation:condition)
  ())

(define-condition sum-argument-must-be-number (type-error loop-runtime-error)
  ())

(define-condition max-argument-must-be-real (type-error loop-runtime-error)
  ())

(define-condition min-argument-must-be-real (type-error loop-runtime-error)
  ())

(define-condition value-must-be-list (type-error loop-runtime-error)
  ())
