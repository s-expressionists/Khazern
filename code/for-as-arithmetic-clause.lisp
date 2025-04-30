(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic (for-as-subclause var-and-type-spec-mixin)
  (;; The order in which the forms are given.  This is a list of three
   ;; elements FROM, TO, and BY in the order that they were given in
   ;; the clause.
   (%order :initarg :order :reader order)
   ;; The form that was given after one of the LOOP keywords FROM,
   ;; UPFROM, or DOWNFROM, or 0 if none of these LOOP keywords was
   ;; given.
   (%start-form :initform 0 :initarg :start-form :reader start-form)
   (%start-var :initform (gensym) :reader start-var)
   ;; The form that was after one of the LOOP keywords TO, UPTO,
   ;; DOWNTO, BELOW, or ABOVE, or NIL if none of these LOOP keywords
   ;; was given.
   (%end-form :initform nil :initarg :end-form :reader end-form)
   (%end-var :initform (gensym) :reader end-var)
   ;; The form that was after the LOOP keyword BY, or 0 if this
   ;; keyword was not given.
   (%by-form :initform 1 :initarg :by-form :reader by-form)
   (%by-var :initform (gensym) :reader by-var)
   ;; If termination is TO, UPTO, or DOWNTO, then this slot contains
   ;; the symbol <=.  If termination is ABOVE or BELOW, then this slot
   ;; contains the symbol <.  If there is TO/UPTO/DOWNTO/ABOVE/BELOW,
   ;; then the loop does not terminate because of this clause, and
   ;; then this slot contains NIL.
   (%termination-test :initform nil
                      :initarg :termination-test
                      :reader termination-test)
   ;; This variable is one step ahead of the iteration variable, and
   ;; when the iteration variable is NIL, the value of this variable
   ;; is never assigned to any iteration variable.
   (%temp-var :initform (gensym) :reader temp-var)))

(defclass for-as-arithmetic-up (for-as-arithmetic) ())

(defclass for-as-arithmetic-down (for-as-arithmetic) ())

(defmethod map-variables (function (clause for-as-arithmetic))
  (%map-variables function (var-spec clause) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From a TYPE-SPEC determine a value used for variable
;;; initialization and a type to use in a declaration, and return them
;;; as two values.  The type returned may be different from the
;;; TYPE-SPEC argument because we may not be able to determine a
;;; initialization value that would conform to the TYPE-SPEC, and in
;;; that case, we must modify the type so that it covers the
;;; initialization value that we give.
;;;
;;; Perhaps this code should be moved to the code utilities module.

(defun arithmetic-value-and-type (type-spec)
  (cond ((eq type-spec 'fixnum)
         (values 0 type-spec))
        ((eq type-spec 'float)
         (values 0.0 type-spec))
        ((eq type-spec 'short-float)
         (values 0s0 type-spec))
        ((eq type-spec 'single-float)
         (values 0f0 type-spec))
        ((eq type-spec 'double-float)
         (values 0d0 type-spec))
        ((eq type-spec 'long-float)
         (values 0l0 type-spec))
        ((and (consp type-spec)
              (eq (car type-spec) 'integer)
              (consp (cdr type-spec))
              (integerp (cadr type-spec)))
         (values (cadr type-spec) type-spec))
        ;; We could add some more here, for instance intervals
        ;; of floats.
        (t
         (values 0 't))))

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
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order :from-to-by))

(defun from-by-to (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order :from-by-to))

(defun to-from-by (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order :to-from-by))

(defun to-by-from (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order :to-by-from))

(defun by-from-to (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order :by-from-to))

(defun by-to-from (x y)
  `(,(or (car x) (car y)) ,@(cdr x) ,@(cdr y) :order :by-to-from))

(define-parser for-as-arithmetic-from ()
  (consecutive #'splice
               'from-parser
               (optional '(nil :order :from-to-by)
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
               (optional '(nil :order :from-to-by)
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
               (optional '(nil :order :from-to-by)
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
               (optional '(nil :order :to-from-by)
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
               (optional '(nil :order :to-from-by)
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
               (optional '(nil :order :to-from-by)
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
               (optional '(nil :order :by-from-to)
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
                        :var-spec var-spec
                        :type-spec type-spec
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
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-arithmetic))
  (case (order clause)
    (:from-to-by
     `((,(start-var clause) ,(start-form clause))
       ,@(if (null (end-form clause))
             '()
             `((,(end-var clause) ,(end-form clause))))
       (,(by-var clause) ,(by-form clause))))
    (:from-by-to
     `((,(start-var clause) ,(start-form clause))
       (,(by-var clause) ,(by-form clause))
       ,@(if (null (end-form clause))
             '()
             `((,(end-var clause) ,(end-form clause))))))
    (:to-from-by
     `(,@(if (null (end-form clause))
             '()
             `((,(end-var clause) ,(end-form clause))))
       (,(start-var clause) ,(start-form clause))
       (,(by-var clause) ,(by-form clause))))
    (:to-by-from
     `(,@(if (null (end-form clause))
             '()
             `((,(end-var clause) ,(end-form clause))))
       (,(by-var clause) ,(by-form clause))
       (,(start-var clause) ,(start-form clause))))
    (:by-from-to
     `((,(by-var clause) ,(by-form clause))
       (,(start-var clause) ,(start-form clause))
       ,@(if (null (end-form clause))
             '()
             `((,(end-var clause) ,(end-form clause))))))
    (:by-to-from
     `((,(by-var clause) ,(by-form clause))
       ,@(if (null (end-form clause))
             '()
             `((,(end-var clause) ,(end-form clause))))
       (,(start-var clause) ,(start-form clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute subclause wrapping.

(defmethod wrap-subclause ((subclause for-as-arithmetic) inner-form)
  (if (null (var-spec subclause))
      (wrap-let `((,(temp-var subclause) ,(start-var subclause)))
                '()
                inner-form)
      (wrap-let `((,(temp-var subclause) ,(start-var subclause))
                  (,(var-spec subclause) ,(start-var subclause)))
                `((cl:type ,(type-spec subclause) ,(var-spec subclause)))
                inner-form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the prologue-form.

(defmethod prologue-forms ((clause for-as-arithmetic-up) end-tag)
  (if (null (termination-test clause))
      `((incf ,(temp-var clause) ,(by-var clause)))
      `((unless (,(termination-test clause)
		 ,(temp-var clause)
		 ,(end-var clause))
	  (go ,end-tag))
	(incf ,(temp-var clause) ,(by-var clause)))))

(defmethod prologue-forms ((clause for-as-arithmetic-down) end-tag)
  (if (null (termination-test clause))
      `((decf ,(temp-var clause) ,(by-var clause)))
      `((unless (,(termination-test clause)
		 ,(end-var clause)
		 ,(temp-var clause))
	  (go ,end-tag))
	(decf ,(temp-var clause) ,(by-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the termination-forms.

(defmethod termination-forms ((clause for-as-arithmetic-up) end-tag)
  (if (null (termination-test clause))
      nil
      `((unless (,(termination-test clause)
                 ,(temp-var clause)
                 ,(end-var clause))
          (go ,end-tag)))))

(defmethod termination-forms ((clause for-as-arithmetic-down) end-tag)
  (if (null (termination-test clause))
      nil
      `((unless (,(termination-test clause)
                 ,(end-var clause)
                 ,(temp-var clause))
          (go ,end-tag)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the step-forms.

(defmethod step-forms ((clause for-as-arithmetic-up))
  (if (null (var-spec clause))
      `((incf ,(temp-var clause) ,(by-var clause)))
      `((setq ,(var-spec clause) ,(temp-var clause))
        (incf ,(temp-var clause) ,(by-var clause)))))

(defmethod step-forms ((clause for-as-arithmetic-down))
  (if (null (var-spec clause))
      `((decf ,(temp-var clause) ,(by-var clause)))
      `((setq ,(var-spec clause) ,(temp-var clause))
        (decf ,(temp-var clause) ,(by-var clause)))))
