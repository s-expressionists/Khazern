(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-HASH

(defclass for-as-hash (for-as-subclause)
  ((%hash-table-form :initarg :hash-table-form :reader hash-table-form)
   (%hash-table-var :initform (gensym) :reader hash-table-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-key-var :initform (gensym) :reader temp-key-var)
   (%temp-value-var :initform (gensym) :reader temp-value-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%other-var-spec :initarg :other-var-spec :reader other-var-spec)))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

(defmethod bound-variables ((subclause for-as-hash))
  (nconc (extract-variables (var-spec subclause))
         (extract-variables (other-var-spec subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(define-parser hash-value-other-parser ()
  (list (lambda (other-var)
          other-var)
        (keyword :hash-value)
        'd-var-spec))

(define-parser hash-key-other-parser ()
  (list (lambda (other-var)
          other-var)
        (keyword :hash-key)
        'd-var-spec))

(define-parser for-as-hash-key (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec hash-table-form other)
                 (make-instance 'for-as-hash-key
                                :var-spec var-spec
                                :type-spec type-spec
                                :hash-table-form hash-table-form
                                :other-var-spec other))
               'd-var-spec
               'optional-type-spec
               (keyword :being)
               (keyword :each :the)
               (keyword :hash-key :hash-keys)
               'terminal
               (keyword :in :of)
               'anything
               (optional nil
                         (consecutive #'identity
                                      (keyword :using)
                                      'terminal
                                      'hash-value-other-parser))))

(define-parser for-as-hash-value (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec hash-table-form other)
                 (make-instance 'for-as-hash-value
                                :var-spec var-spec
                                :type-spec type-spec
                                :hash-table-form hash-table-form
                                :other-var-spec other))
               'd-var-spec
               'optional-type-spec
               (keyword :being)
               (keyword :each :the)
               (keyword :hash-value :hash-values)
               'terminal
               (keyword :in :of)
               'anything
               (optional nil
                         (consecutive #'identity
                                      (keyword :using)
                                      'terminal
                                      'hash-key-other-parser))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-hash))
  `((,(hash-table-var clause) ,(hash-table-form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-hash) inner-form)
  `(let ((,(temp-entry-p-var subclause) nil)
         (,(temp-key-var subclause) nil)
         (,(temp-value-var subclause) nil)
         ,.(generate-variable-bindings (var-spec subclause))
         ,.(generate-variable-bindings (other-var-spec subclause)))
     (declare ,@(generate-variable-declarations (var-spec subclause) (type-spec subclause)))
     (with-hash-table-iterator
         (,(iterator-var subclause) ,(hash-table-var subclause))
       ,inner-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod prologue-form ((subclause for-as-hash-key) end-tag)
  `(progn (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))
          (unless ,(temp-entry-p-var subclause)
            (go ,end-tag))
          ,(generate-assignments (var-spec subclause)
                                 (temp-key-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-value-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))

(defmethod prologue-form ((subclause for-as-hash-value) end-tag)
  `(progn (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))
          (unless ,(temp-entry-p-var subclause)
            (go ,end-tag))
          ,(generate-assignments (var-spec subclause)
                                 (temp-value-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-key-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-form ((subclause for-as-hash) end-tag)
  `(unless ,(temp-entry-p-var subclause)
     (go ,end-tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-form ((subclause for-as-hash-key))
  `(progn ,(generate-assignments (var-spec subclause)
                                 (temp-key-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-value-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))

(defmethod step-form ((subclause for-as-hash-value))
  `(progn ,(generate-assignments (var-spec subclause)
                                 (temp-value-var subclause))
          ,(generate-assignments (other-var-spec subclause)
                                 (temp-key-var subclause))
          (multiple-value-bind (entry-p key value)
              (,(iterator-var subclause))
            (setq ,(temp-entry-p-var subclause) entry-p
                  ,(temp-key-var subclause) key
                  ,(temp-value-var subclause) value))))
