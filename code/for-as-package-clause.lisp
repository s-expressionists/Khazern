(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause)
  ((%package-form :initarg :package-form :reader package-form)
   (%package-var :initform (gensym) :reader package-var)
   (%temp-entry-p-var :initform (gensym) :reader temp-entry-p-var)
   (%temp-symbol-var :initform (gensym) :reader temp-symbol-var)
   (%iterator-var :initform (gensym) :reader iterator-var)
   (%iterator-keywords :initarg :iterator-keywords :reader iterator-keywords)))

(defmethod bound-variables ((subclause for-as-package))
  (extract-variables (var-spec subclause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(define-parser for-as-package-parser (:for-as-subclause)
  (consecutive (lambda (var-spec
                        type-spec
                        iterator-keywords
                        package-form)
                 (make-instance 'for-as-package
                                :var-spec var-spec
                                :type-spec type-spec
                                :package-form package-form
                                :iterator-keywords iterator-keywords))
               'anything
               'optional-type-spec
               (keyword :being)
               (keyword :each :the)
               (alternative (consecutive (lambda ()
                                           '(:internal :external :inherited))
                                         (keyword :symbol :symbols))
                            (consecutive (lambda ()
                                           '(:internal :external))
                                         (keyword :present-symbol :present-symbols))
                            (consecutive (lambda ()
                                           '(:external))
                                         (keyword :external-symbol :external-symbols)))
               'terminal
               (optional '*package*
                         (consecutive #'identity
                                      (keyword :in :of)
                                      'anything))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-package))
  `((,(package-var clause) ,(package-form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-package) inner-form)
  (wrap-let `((,(temp-entry-p-var subclause) nil)
              (,(temp-symbol-var subclause) nil)
              ,.(generate-variable-bindings (var-spec subclause)))
            (generate-variable-declarations (var-spec subclause) (type-spec subclause))
            `((with-package-iterator
                  (,(iterator-var subclause)
                   ,(package-var subclause)
                   ,@(iterator-keywords subclause))
                ,@inner-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod prologue-forms ((subclause for-as-package) end-tag)
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,end-tag))
    ,@(generate-assignments (var-spec subclause)
                            (temp-symbol-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination form.

(defmethod termination-forms ((subclause for-as-package) end-tag)
  `((unless ,(temp-entry-p-var subclause)
      (go ,end-tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod step-forms ((subclause for-as-package))
  `(,@(generate-assignments (var-spec subclause)
                            (temp-symbol-var subclause))
    (multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))))
