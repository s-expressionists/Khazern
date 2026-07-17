(cl:in-package #:khazern)

(define-condition loop-clause-condition (acclimation:condition)
  ((%clause :reader clause
            :initarg :clause
            :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for parsing
;;;

(define-condition loop-parse-style-warning (style-warning loop-clause-condition) ())

(define-condition invalid-preposition-order (loop-parse-style-warning)
  ((%name :reader name
          :initarg :name)
   (%inclusive :reader inclusivep
               :initarg :inclusive
               :initform nil)
   (%first-preposition :reader first-preposition
                       :initarg :first-preposition)
   (%second-preposition :reader second-preposition
                        :initarg :second-preposition)))

(define-condition loop-parse-error (parse-error loop-clause-condition) ())

(define-condition loop-parse-error-found (loop-parse-error)
  ((%found :reader found
           :initarg :found)))

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

(define-condition unexpected-token-found (loop-parse-error-found)
  ())

(define-condition conflicting-stepping-directions (loop-parse-error)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions for syntactic and semantic analysis
;;;
;;; FIXME: Define English language condition reporters for these
;;;        conditions.

(define-condition loop-syntax-style-warning (style-warning acclimation:condition) ())

(define-condition possible-invalid-clause-order
    (loop-syntax-style-warning)
  ((%clause :reader clause
            :initarg :clause)
   (%found-group :reader found-group
                 :initarg :found-group)
   (%expected-group :reader expected-group
                    :initarg :expected-group)))

;;; The root of all syntax errors.
(define-condition loop-syntax-error (program-error acclimation:condition)
  ())

;;; This condition is signaled when a NIL is found
;;; in a simple LOOP
(define-condition non-compound-form (loop-syntax-error) ())

;;; This condition is signaled when the first clause is a name-clause
;;; but there are other name-clauses. 
(define-condition multiple-name-clauses (loop-syntax-error)
  ((%clauses :reader clauses
             :initarg :clauses)))

(define-condition invalid-clause-order (loop-syntax-error)
  ((%clause :reader clause
            :initarg :clause)
   (%found-group :reader found-group
                 :initarg :found-group)
   (%expected-group :reader expected-group
                    :initarg :expected-group)))

;;; This condition is signaled when there are multiple occurrences of
;;; a variable to be bound by any loop clause.
(define-condition multiple-variable-occurrences (loop-syntax-error)
  ((%bound-variable :reader bound-variable
                    :initarg :bound-variable)))

;;; This condition is signaled when there are overlaps betweeen
;;; iteration and accumulation variables.
(define-condition iteration-accumulation-overlap (loop-syntax-error)
  ((%bound-variable :reader bound-variable
                    :initarg :bound-variable)))

;;; This condition is signaled when there are multiple occurrences of
;;; a variable used in accumulation clauses.
(define-condition multiple-accumulation-occurrences (loop-syntax-error)
  ((%bound-variable :reader bound-variable
                    :initarg :bound-variable)
   (%first-clause :reader first-clause
                  :initarg :first-clause)
   (%second-clause :reader second-clause
                   :initarg :second-clause)))

(define-condition non-nullable-simple-d-var-spec (loop-syntax-error)
  ((%var-spec :reader var-spec
              :initarg :var-spec)))

(define-condition invalid-multiple-values-d-var-spec (loop-syntax-error)
  ((%var-spec :reader var-spec
              :initarg :var-spec)))

(define-condition loop-semantic-style-warning (style-warning acclimation:condition) ())

(define-condition unable-to-deduce-initial-value (loop-semantic-style-warning)
  ((%type-spec :reader type-spec
               :initarg :type-spec
               :initform nil)))

(define-condition conflicting-types (loop-semantic-style-warning)
  ((%name :reader name
          :initarg :name)
   (%type1 :reader type1
           :initarg :type1
           :initform nil)
   (%type2 :reader type2
           :initarg :type2
           :initform nil)
   (%replacement-type :reader replacement-type
                      :initarg :replacement-type
                      :initform nil)))

;;; The root of all semantic errors.
(define-condition loop-semantic-error (program-error acclimation:condition)
  ())

(define-condition invalid-data-type (loop-semantic-error)
  ((%subtype :reader subtype
             :initarg :subtype)
   (%supertype :reader supertype
               :initarg :supertype)))

(define-condition unknown-data-type (loop-semantic-error)
  ((%subtype :reader subtype
             :initarg :subtype)
   (%supertype :reader supertype
               :initarg :supertype)))

(defun check-subtype (subtype supertype)
  (multiple-value-bind (subtype-p valid-p)
      (subtypep subtype supertype)
    (cond ((not valid-p)
	   (warn 'unknown-data-type
                 :subtype subtype :supertype supertype))
	  ((not subtype-p)
	   (error 'invalid-data-type
                  :subtype subtype :supertype supertype)))))

(defun check-single-values-var-spec (d-spec)
  (when (typep d-spec 'values-binding)
    (error 'invalid-multiple-values-d-var-spec
           :var-spec `(values ,@(mapcar #'var-spec (var-spec d-spec))))))

(defun check-nullable-simple-var-spec (d-spec)
  (check-single-values-var-spec d-spec)
  (unless (and (symbolp (var-spec d-spec))
               (or (not (constantp (var-spec d-spec)))
                   (null (var-spec d-spec))))
    (error 'non-nullable-simple-d-var-spec :var-spec (var-spec d-spec))))
