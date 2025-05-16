(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-CLAUSE.
;;;
;;; The HyperSpec says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;    for-as-clause ::= {for | as} for-as-subclause {and for-as-subclause}* 
;;;    for-as-subclause::= for-as-arithmetic | for-as-in-list | 
;;;                        for-as-on-list | for-as-equals-then | 
;;;                        for-as-across | for-as-hash | for-as-package 
;;;
;;; For the purpose of specialization, we need different names for the
;;; main clauses as well as for the subclauses, so we alter this
;;; grammar a bit and define it like this instead:
;;;
;;;    for-as-clause::= 
;;;      for-as-arithmetic-clause | for-as-in-list-clause | 
;;;      for-as-on-list-clause | for-as-equals-then-clause | 
;;;      for-as-across-clause | for-as-hash-clause | for-as-package-clause
;;;    
;;;    for-as-arithmetic-clause ::=
;;;      {for | as} for-as-arithmetic {and for-as-subclause}* 
;;;    
;;;    for-as-in-list-clause ::=
;;;      {for | as} for-as-in-list {and for-as-subclause}* 
;;;    
;;;    for-as-on-list-clause ::=
;;;      {for | as} for-as-on-list {and for-as-subclause}* 
;;;    
;;;    for-as-equals-then-clause ::=
;;;      {for | as} for-as-equals-then {and for-as-subclause}* 
;;;    
;;;    for-as-across-clause ::=
;;;      {for | as} for-as-across {and for-as-subclause}* 
;;;
;;;    for-as-hash-clause ::=
;;;      {for | as} for-as-hash {and for-as-subclause}* 
;;;
;;;    for-as-package-clause ::=
;;;      {for | as} for-as-package {and for-as-subclause}* 

(defclass for-as-clause (variable-clause subclauses-mixin)
  ())

(defclass for-as-subclause (var-mixin)
  ())

(defmethod map-variables (function (clause for-as-clause))
  (map-variables function (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parse a FOR-AS clause.

(define-parser for-as-subclause+ ()
  (delimited-list-by-category :for-as-subclause nil 'and))
  
(define-parser for-as-clause (:body-clause)
  (consecutive (lambda (subclauses)
                 (make-instance 'for-as-clause :subclauses subclauses))
               (keyword :for :as)
               'for-as-subclause+))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic (for-as-subclause)
  (;; The order in which the forms are given.  This is a list of three
   ;; elements FROM, TO, and BY in the order that they were given in
   ;; the clause.
   (%order :reader order
           :initarg :order)
   ;; The form that was given after one of the LOOP keywords FROM,
   ;; UPFROM, or DOWNFROM, or 0 if none of these LOOP keywords was
   ;; given.
   (%start-form :accessor start-form
                :initarg :start-form
                :initform 0)
   ;; The form that was after one of the LOOP keywords TO, UPTO,
   ;; DOWNTO, BELOW, or ABOVE, or NIL if none of these LOOP keywords
   ;; was given.
   (%end-form :accessor end-form
              :initarg :end-form
              :initform nil)
   ;; The form that was after the LOOP keyword BY, or 0 if this
   ;; keyword was not given.
   (%by-form :accessor by-form
             :initarg :by-form
             :initform 1)
   ;; This variable is one step ahead of the iteration variable, and
   ;; when the iteration variable is NIL, the value of this variable
   ;; is never assigned to any iteration variable.
   (%next-var :reader next-var
              :initform (make-instance 'd-spec
                                       :var-spec (gensym "NEXT")
                                       :type-spec 'number))
   (%end-var :reader end-var
             :initform (make-instance 'd-spec
                                      :var-spec (gensym "END")
                                      :type-spec 'number))
   (%by-var :reader by-var
            :initform (make-instance 'd-spec
                                     :var-spec (gensym "BY")
                                     :type-spec 'number))
   ;; If termination is TO, UPTO, or DOWNTO, then this slot contains
   ;; the symbol <=.  If termination is ABOVE or BELOW, then this slot
   ;; contains the symbol <.  If there is TO/UPTO/DOWNTO/ABOVE/BELOW,
   ;; then the loop does not terminate because of this clause, and
   ;; then this slot contains NIL.
   (%termination-test :reader termination-test
                      :initarg :termination-test
                      :initform nil)))

(defclass for-as-arithmetic-up (for-as-arithmetic)
  ())

(defclass for-as-arithmetic-down (for-as-arithmetic)
  ())

(defmethod map-variables (function (clause for-as-arithmetic))
  (map-variables function (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers for individual keywords.

(define-parser from-parser ()
  (consecutive (lambda (form)
                 `(nil :start-form ,form))
               (keyword :from)
               'terminal
               'anything))

(define-parser upfrom-parser ()
  (consecutive (lambda (form)
                 `(for-as-arithmetic-up :start-form ,form))
               (keyword :upfrom)
               'terminal
               'anything))

(define-parser downfrom-parser ()
  (consecutive (lambda (form)
                 `(for-as-arithmetic-down :start-form ,form))
               (keyword :downfrom)
               'terminal
               'anything))

(define-parser to-parser ()
  (consecutive (lambda (form)
                 `(nil :termination-test <= :end-form ,form))
               (keyword :to)
               'terminal
               'anything))

(define-parser upto-parser ()
  (consecutive (lambda (test form)
                 `(for-as-arithmetic-up :termination-test ,test :end-form ,form))
               (alternative (consecutive (constantly '<=)
                                         (keyword :upto))
                            (consecutive (constantly '<)
                                         (keyword :below)))
               'terminal
               'anything))

(define-parser downto-parser ()
  (consecutive (lambda (test form)
                 `(for-as-arithmetic-down :termination-test ,test :end-form ,form))
               (alternative (consecutive (constantly '<=)
                                         (keyword :downto))
                            (consecutive (constantly '<)
                                         (keyword :above)))
               'terminal
               'anything))

(define-parser by-parser ()
  (consecutive (lambda (form)
                 `(nil :by-form ,form))
               (keyword :by)
               'terminal
               'anything))

(defun splice (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y)))

(defun from-to-by (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order (:from :to :by)))

(defun from-by-to (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order (:from :by :to)))

(defun to-from-by (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order (:to :from :by)))

(defun to-by-from (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order (:to :by :from)))

(defun by-from-to (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order (:by :from :to)))

(defun by-to-from (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order (:by :to :from)))

(define-parser for-as-arithmetic-from ()
  (consecutive #'splice
               'from-parser
               (optional '(nil :order (:from :to :by))
                         (alternative (consecutive #'from-to-by
                                                   (alternative 'to-parser
                                                                'upto-parser
                                                                'downto-parser)
                                                   (optional nil
                                                             'by-parser))
                                      (consecutive #'from-by-to
                                                   'by-parser
                                                   (optional nil
                                                             (alternative 'to-parser
                                                                          'upto-parser
                                                                          'downto-parser)))))))

(define-parser for-as-arithmetic-upfrom ()
  (consecutive #'splice
               'upfrom-parser
               (optional '(nil :order (:from :to :by))
                         (alternative (consecutive #'from-to-by
                                                   (alternative 'to-parser
                                                                'upto-parser)
                                                   (optional nil
                                                             'by-parser))
                                      (consecutive #'from-by-to
                                                   'by-parser
                                                   (optional nil
                                                             (alternative 'to-parser
                                                                          'upto-parser)))))))

(define-parser for-as-arithmetic-downfrom ()
  (consecutive #'splice
               'downfrom-parser
               (optional '(nil :order (:from :to :by))
                         (alternative (consecutive #'from-to-by
                                                   (alternative 'to-parser
                                                                'downto-parser)
                                                   (optional nil
                                                             'by-parser))
                                      (consecutive #'from-by-to
                                                   'by-parser
                                                   (optional nil
                                                             (alternative 'to-parser
                                                                          'downto-parser)))))))

(define-parser for-as-arithmetic-to ()
  (consecutive #'splice
               'to-parser
               (optional '(nil :order (:to :from :by))
                         (alternative (consecutive #'to-from-by
                                                   (alternative 'from-parser
                                                                'upfrom-parser
                                                                'downfrom-parser)
                                                   (optional nil
                                                             'by-parser))
                                      (consecutive #'to-by-from
                                                   'by-parser
                                                   (optional nil
                                                             (alternative 'from-parser
                                                                          'upfrom-parser
                                                                          'downfrom-parser)))))))

(define-parser for-as-arithmetic-upto ()
  (consecutive #'splice
               'upto-parser
               (optional '(nil :order (:to :from :by))
                         (alternative (consecutive #'to-from-by
                                                   (alternative 'from-parser
                                                                'upfrom-parser)
                                                   (optional nil
                                                             'by-parser))
                                      (consecutive #'to-by-from
                                                   'by-parser
                                                   (optional nil
                                                             (alternative 'from-parser
                                                                          'upfrom-parser)))))))

(define-parser for-as-arithmetic-downto ()
  (consecutive #'splice
               'downto-parser
               (optional '(nil :order (:to :from :by))
                         (alternative (consecutive #'to-from-by
                                                   (alternative 'from-parser
                                                                'downfrom-parser)
                                                   (optional nil
                                                             'by-parser))
                                      (consecutive #'to-by-from
                                                   'by-parser
                                                   (optional nil
                                                             (alternative 'from-parser
                                                                          'downfrom-parser)))))))

(define-parser for-as-arithmetic-by ()
  (consecutive #'splice
               'by-parser
               (optional '(nil :order (:by :from :to))
                         (alternative (consecutive #'by-from-to
                                                   'from-parser
                                                   (optional nil
                                                             (alternative 'to-parser
                                                                          'upto-parser
                                                                          'downto-parser)))
                                      (consecutive #'by-from-to
                                                   'upfrom-parser
                                                   (optional nil
                                                             (alternative 'to-parser
                                                                          'upto-parser)))
                                      (consecutive #'by-from-to
                                                   'downfrom-parser
                                                   (optional nil
                                                             (alternative 'to-parser
                                                                          'downto-parser)))
                                      (consecutive #'by-to-from
                                                   'to-parser
                                                   (optional nil
                                                             (alternative 'from-parser
                                                                          'upfrom-parser
                                                                          'downfrom-parser)))
                                      (consecutive #'by-to-from
                                                   'upto-parser
                                                   (optional nil
                                                             (alternative 'from-parser
                                                                          'upfrom-parser)))
                                      (consecutive #'by-to-from
                                                   'downto-parser
                                                   (optional nil
                                                             (alternative 'from-parser
                                                                          'downfrom-parser)))))))

(define-parser for-as-arithmetic-parser (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec g)
                 (apply #'make-instance
                        (or (car g) 'for-as-arithmetic-up)
                        :var (make-instance 'd-spec
                                            :var-spec var-spec
                                            :type-spec type-spec)
                        (cdr g)))
               'd-var-spec
               'optional-type-spec
               (alternative 'for-as-arithmetic-from
                            'for-as-arithmetic-upfrom
                            'for-as-arithmetic-downfrom
                            'for-as-arithmetic-to
                            'for-as-arithmetic-upto
                            'for-as-arithmetic-downto
                            'for-as-arithmetic-by)))

(defmethod analyze ((clause for-as-arithmetic))
  (with-accessors ((next-var next-var)
                   (by-var by-var)
                   (var var)
                   (start-form start-form)
                   (by-form by-form)
                   (end-form end-form))
      clause
    (with-accessors ((var-type type-spec))
        var
      (with-accessors ((next-type type-spec))
          next-var
        (with-accessors ((by-type type-spec))
            by-var
          (cond ((not (eq var-type t))
                 (setf next-type (numeric-super-type var-type)
                       by-type next-type)
                 (when (numberp start-form)
                   (setf start-form (coerce start-form next-type)))
                 (when (numberp by-form)
                   (setf by-form (coerce by-form by-type))))
                ((numberp start-form)
                 (let ((val start-form))
                   (when (numberp by-form)
                     (incf val by-form))
                   (setf next-type (numeric-type-of val)
                         by-type next-type
                         var-type next-type))
                 (setf start-form (coerce start-form next-type))
                 (when (numberp by-form)
                   (setf by-form (coerce by-form by-type))))))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-arithmetic))
  (nconc (mapcan (lambda (name)
                   (ecase name
                     (:from
                      (d-spec-simple-bindings (next-var clause) (start-form clause)))
                     (:to
                      (when (and (end-form clause)
                                     (not (numberp (end-form clause))))
                        (d-spec-simple-bindings (end-var clause) (end-form clause))))
                     (:by
                      (unless (numberp (by-form clause))
                        (d-spec-simple-bindings (by-var clause) (by-form clause))))))
                 (order clause))
         (d-spec-simple-bindings (var clause))))

(defmethod initial-declarations ((clause for-as-arithmetic))
  (nconc (d-spec-simple-declarations (next-var clause))
         (unless (numberp (by-form clause))
           (d-spec-simple-declarations (by-var clause)))
         (unless (or (null (end-form clause))
                     (numberp (end-form clause)))
           (d-spec-simple-declarations (end-var clause)))
         (d-spec-simple-declarations (var clause) :ignorable t :nullable t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial-step-form.

(defmethod initial-step-forms ((clause for-as-arithmetic-up))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(var-spec (next-var clause))
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (var-spec (end-var clause))))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defmethod initial-step-forms ((clause for-as-arithmetic-down))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (var-spec (end-var clause)))
                      ,(var-spec (next-var clause)))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent-step-forms.

(defmethod subsequent-step-forms ((clause for-as-arithmetic-up))
  (nconc `((incf ,(var-spec (next-var clause))
                 ,(if (numberp (by-form clause))
                      (by-form clause)
                      (var-spec (by-var clause)))))
         (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(var-spec (next-var clause))
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (var-spec (end-var clause))))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defmethod subsequent-step-forms ((clause for-as-arithmetic-down))
  (nconc `((decf ,(var-spec (next-var clause))
                 ,(if (numberp (by-form clause))
                      (by-form clause)
                      (var-spec (by-var clause)))))
         (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (var-spec (end-var clause)))
                      ,(var-spec (next-var clause)))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defclass for-as-list (for-as-subclause form-mixin form-var-mixin)
  ((%by-form :reader by-form
             :initarg :by-form)
   (%by-var :reader by-var
            :initform (gensym))
   (%rest-var :reader rest-var
              :initform (gensym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-IN-LIST.

(defclass for-as-in-list (for-as-list) ())

(define-parser for-as-list-by-parser ()
  (optional '#'cdr
            (consecutive #'identity
                         (keyword :by)
                         'anything)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-in-list-parser (:for-as-subclause)
  (consecutive (lambda (var type-spec form by-form)
                 (make-instance 'for-as-in-list
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec)
                                :form form
                                :by-form by-form))
               'd-var-spec
               'optional-type-spec
               (keyword :in)
               'terminal
               'anything
               'for-as-list-by-parser))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ON-LIST.

(defclass for-as-on-list (for-as-list) ())

(define-parser for-as-on-list-parser (:for-as-subclause)
  (consecutive (lambda (var type-spec form by-form)
                 (make-instance 'for-as-on-list
                                :var (make-instance 'd-spec
                                                    :var-spec var
                                                    :type-spec type-spec)
                                :form form
                                :by-form by-form))
               'd-var-spec
               'optional-type-spec
               (keyword :on)
               'terminal
               'anything
               'for-as-list-by-parser))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-list))
  `((,(form-var clause) ,(form clause))
    ,@(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
          '()
          `((,(by-var clause) ,(by-form clause))))))

(defmethod final-bindings ((clause for-as-list))
  `((,(rest-var clause) ,(form-var clause))
    ,.(d-spec-outer-bindings (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod final-declarations ((clause for-as-list))
  (d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial step forms.

(defmethod initial-step-forms ((clause for-as-in-list))
  `((when (endp ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) `(car ,(rest-var clause)))))

(defmethod initial-step-forms ((clause for-as-on-list))
  `((when (atom ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) (rest-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent step forms.

(defmethod subsequent-step-forms ((clause for-as-in-list))
  `(,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))
    (when (endp ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) `(car ,(rest-var clause)))))

(defmethod subsequent-step-forms ((clause for-as-on-list))
  `(,(if (member (by-form clause) '(#'cdr #'cddr) :test #'equal)
         `(setq ,(rest-var clause)
                (,(cadr (by-form clause)) ,(rest-var clause)))
         `(setq ,(rest-var clause)
                (funcall ,(by-var clause) ,(rest-var clause))))
    (when (atom ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) (rest-var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-EQUALS-THEN.

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :reader initial-form
                  :initarg :initial-form)
   (%subsequent-form :reader subsequent-form
                     :initarg :subsequent-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser for-as-equals-then-parser (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec form1 initargs)
                 (apply #'make-instance 'for-as-equals-then
                        :var (make-instance 'd-spec
                                            :var-spec var-spec
                                            :type-spec type-spec
                                            :temp-var t)
                        :initial-form form1
                        (or initargs (cl:list :subsequent-form form1))))
               'd-var-spec
               'optional-type-spec
               (keyword :=)
               'terminal
               'anything
               (optional nil
                         (consecutive (lambda (form)
                                        (cl:list :subsequent-form form))
                                      (keyword :then)
                                      'terminal
                                      'anything))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-equals-then))
  (d-spec-outer-bindings (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause for-as-equals-then))
  (d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-bindings.

(defmethod initial-step-bindings ((clause for-as-equals-then))
  (d-spec-inner-bindings (var clause) (initial-form clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod initial-step-forms ((clause for-as-equals-then))
  `((setq ,@(d-spec-inner-assignments (var clause) (initial-form clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-bindings.

(defmethod subsequent-step-bindings ((clause for-as-equals-then))
  (d-spec-inner-bindings (var clause) (subsequent-form clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod subsequent-step-forms ((clause for-as-equals-then))
  `((setq ,@(d-spec-inner-assignments (var clause) (subsequent-form clause)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ACROSS

(defclass for-as-across (for-as-subclause form-mixin form-var-mixin)
  ((%length-var :reader length-var
                :initform (gensym "LENGTH"))
   (%index-var :reader index-var
               :initform (gensym "INDEX"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(define-parser for-as-across (:for-as-subclause)
  (consecutive (lambda (var-spec type-spec form)
                 (make-instance 'for-as-across
                                :var (make-instance 'd-spec
                                                    :var-spec var-spec
                                                    :type-spec type-spec)
                                :form form))
               'd-var-spec
               'optional-type-spec
               (keyword :across)
               'terminal
               'anything))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute bindings.

(defmethod initial-bindings ((clause for-as-across))
  `((,(form-var clause) ,(form clause))
    (,(index-var clause) 0)))

(defmethod final-bindings ((clause for-as-across))
  `((,(length-var clause) (length ,(form-var clause)))
    ,.(d-spec-outer-bindings (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute declarations.

(defmethod final-declarations ((clause for-as-across))
  (d-spec-outer-declarations (var clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod initial-step-forms ((clause for-as-across))
  `((when (>= ,(index-var clause) ,(length-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute step-forms.

(defmethod subsequent-step-forms ((clause for-as-across))
  `((when (>= (incf ,(index-var clause)) ,(length-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PATH

(defclass for-as-path (for-as-subclause)
  ())

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-HASH

(defclass for-as-hash (for-as-subclause form-mixin form-var-mixin)
  ((%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-key-var :reader temp-key-var
                  :initform (gensym))
   (%temp-value-var :reader temp-value-var
                    :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%other-var :accessor other-var
               :initarg :other-var
               :initform (make-instance 'd-spec
                                        :var-spec nil))))

(defclass for-as-hash-key (for-as-hash) ())

(defclass for-as-hash-value (for-as-hash) ())

(defmethod map-variables :after (function (clause for-as-hash))
  (map-variables function (other-var clause)))

(defun make-for-as-hash-key (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-hash-key
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)))

(defun make-for-as-hash-value (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-hash-value
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)))

(defmethod path-preposition-names ((instance for-as-hash))
  '((:in . :in) (:of . :in)))

(defmethod (setf path-preposition) (expression (instance for-as-hash) (key (eql :in)))
  (setf (form instance) expression))

(defmethod path-using-names ((instance for-as-hash-key))
  '((:hash-value . :other) (:hash-values . :other)))

(defmethod path-using-names ((instance for-as-hash-value))
  '((:hash-key . :other) (:hash-keys . :other)))

(defmethod (setf path-using) (value (instance for-as-hash) (key (eql :other)))
  (setf (other-var instance) (make-instance 'd-spec
                                            :var-spec value))
  value)

(defmethod analyze ((clause for-as-hash))
  (unless (slot-boundp clause '%form)
    (error "Missing IN/OF preposition")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-hash))
  `((,(form-var clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-hash) inner-form)
  (wrap-let `((,(temp-entry-p-var subclause) nil)
              (,(temp-key-var subclause) nil)
              (,(temp-value-var subclause) nil)
              ,.(d-spec-outer-bindings (var subclause))
              ,.(d-spec-outer-bindings (other-var subclause)))
            (d-spec-outer-declarations (var subclause))
            `((with-hash-table-iterator
                  (,(iterator-var subclause) ,(form-var subclause))
                ,@inner-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod initial-step-forms ((subclause for-as-hash-key))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-key-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-value-var subclause))))

(defmethod initial-step-forms ((subclause for-as-hash-value))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-value-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-key-var subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod subsequent-step-forms ((subclause for-as-hash-key))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-key-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-value-var subclause))))

(defmethod subsequent-step-forms ((subclause for-as-hash-value))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-key-var subclause)
                          ,(temp-value-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-value-var subclause))
    ,@(d-spec-inner-form (other-var subclause)
                         (temp-key-var subclause))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PACKAGE

(defclass for-as-package (for-as-subclause form-mixin form-var-mixin)
  ((%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-symbol-var :reader temp-symbol-var
                     :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form '*package*))

(defun make-for-as-package-symbol (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-package
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :iterator-keywords '(:internal :external :inherited)))

(defun make-for-as-package-present-symbol (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-package
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :iterator-keywords '(:internal :external)))

(defun make-for-as-package-external-symbol (name var-spec type-spec)
  (declare (ignore name))
  (make-instance 'for-as-package
                 :var (make-instance 'd-spec
                                     :var-spec var-spec
                                     :type-spec type-spec)
                 :iterator-keywords '(:external)))

(defmethod path-preposition-names ((instance for-as-package))
  '((:in . :in) (:of . :in)))

(defmethod (setf path-preposition) (expression (instance for-as-package) (key (eql :in)))
  (setf (form instance) expression))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial bindings.

(defmethod initial-bindings ((clause for-as-package))
  `((,(form-var clause) ,(form clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause for-as-package) inner-form)
  (wrap-let `((,(temp-entry-p-var subclause) nil)
              (,(temp-symbol-var subclause) nil)
              ,.(d-spec-outer-bindings (var subclause)))
            (d-spec-outer-declarations (var subclause))
            `((with-package-iterator
                  (,(iterator-var subclause)
                   ,(form-var subclause)
                   ,@(iterator-keywords subclause))
                ,@inner-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue form.

(defmethod initial-step-forms ((subclause for-as-package))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-symbol-var subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step form.

(defmethod subsequent-step-forms ((subclause for-as-package))
  `((multiple-value-setq (,(temp-entry-p-var subclause)
                          ,(temp-symbol-var subclause))
      (,(iterator-var subclause)))
    (unless ,(temp-entry-p-var subclause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var subclause)
                         (temp-symbol-var subclause))))



;;; Clause WITH-CLAUSE.
;;;
;;; A WITH-CLAUSE allows the creation of local variables.  It is
;;; executed once.
;;;
;;; The syntax of a with-clause is:
;;;
;;;    with-clause ::= WITH var1 [type-spec] [= form1]
;;;                    {AND var2 [type-spec] [= form2]}*
;;;
;;; where var1 and var2 are destructuring variable specifiers
;;; (d-var-spec) allowing multiple local variables to be created in a
;;; single with-clause by destructuring the value of the corresponding
;;; form.
;;;
;;; When there are several consecutive with-clause, the execution is
;;; done sequentially, so that variables created in one with-clause
;;; can be used in the forms of subsequent with-clauses.  If parallel
;;; creation of variables is wanted, then the with-clause can be
;;; followed by one or more and-clauses.
;;;
;;; The (destructuring) type specifier is optional.  If no type
;;; specifier is given, it is as if t was given.
;;;
;;; The initialization form is optional.  If there is a corresponding
;;; type specifier for a variable, but no initialization form, then
;;; the variable is initialized to a value that is appropriate for the
;;; type.  In particular, for the type t the value is nil, for the
;;; type number, the value is 0, and for the type float, the value is
;;; 0.0.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class WITH-CLAUSE.
;;;

(defclass with-clause (variable-clause subclauses-mixin)
  ())

(defclass with-subclause (var-mixin)
  ())

(defclass with-subclause-no-form (with-subclause)
  ())

(defclass with-subclause-with-form (with-subclause form-mixin form-var-mixin)
  ())

(defmethod map-variables (function (clause with-clause))
  (map-variables function (subclauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser with-subclause ()
  (consecutive (lambda (var-spec type-spec initargs)
                 (apply #'make-instance (if initargs
                                            'with-subclause-with-form
                                            'with-subclause-no-form)
                        :var (make-instance 'd-spec
                                            :var-spec var-spec
                                            :type-spec type-spec)
                        initargs))
               'anything
               'optional-type-spec
               (optional nil
                         (consecutive (lambda (form)
                                        `(:form ,form))
                                      (keyword :=)
                                      'anything))))

(define-parser with-clause (:body-clause)
  (consecutive (lambda (first rest)
                 (make-instance 'with-clause
                                :subclauses (cons first rest)))
               (keyword :with)
               'terminal
               'with-subclause
               (repeat* #'cl:list
                        (consecutive #'identity
                                     (keyword :and)
                                     'terminal
                                     'with-subclause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause with-subclause-with-form))
  (list* `(,(form-var clause) ,(form clause))
         (d-spec-outer-bindings (var clause))))

(defmethod initial-bindings ((clause with-subclause-no-form))
  (let ((result '()))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                   (push `(,var ,(if (subtypep type 'number)
                               (coerce 0 type)
                               nil)) result))
                   (var clause))
    (nreverse result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subclause wrapper.

(defmethod wrap-subclause ((subclause with-subclause-with-form) inner-form)
  (nconc (d-spec-inner-form (var subclause) (form-var subclause))
         inner-form))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the declarations.

(defmethod initial-declarations ((clause with-subclause))
  (d-spec-outer-declarations (var clause)))
