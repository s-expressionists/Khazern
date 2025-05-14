(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-ARITHMETIC.

(defclass for-as-arithmetic (for-as-subclause)
  ((%order :reader order
           :initarg :order
           :documentation "The order in which the forms are given.  This is a list of three
elements FROM, TO, and BY in the order that they were given in the
clause.")
   (%start-form :accessor start-form
                :initarg :start-form
                :initform 0
                :documentation "The form that was given after one of the LOOP keywords FROM, UPFROM,
or DOWNFROM, or 0 if none of these LOOP keywords was given.")
   (%next-var :initform (gensym "NEXT") :reader next-var)
   ;; The form that was after one of the LOOP keywords TO, UPTO,
   ;; DOWNTO, BELOW, or ABOVE, or NIL if none of these LOOP keywords
   ;; was given.
   (%end-form :initform nil :initarg :end-form :accessor end-form)
   (%end-var :initform (gensym "END") :reader end-var)
   ;; The form that was after the LOOP keyword BY, or 0 if this
   ;; keyword was not given.
   (%by-form :initform 1 :initarg :by-form :accessor by-form)
   (%by-var :initform (gensym "BY") :reader by-var)
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
   (%temp-var :reader temp-var :initform (gensym))
   (%temp-type :accessor temp-type :initform 'number)))

(defclass for-as-arithmetic-up (for-as-arithmetic) ())

(defclass for-as-arithmetic-down (for-as-arithmetic) ())

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
  (with-accessors ((temp-type temp-type)
                   (var var)
                   (start-form start-form)
                   (by-form by-form)
                   (end-form end-form))
      clause
    (with-accessors ((type-spec type-spec))
        var
      (cond ((not (eq type-spec t))
             (setf temp-type (numeric-super-type type-spec))
             (when (numberp start-form)
               (setf start-form (coerce start-form temp-type)))
             (when (numberp by-form)
               (setf by-form (coerce by-form temp-type))))
            ((numberp start-form)
             (let ((val start-form))
               (when (numberp by-form)
                 (incf val by-form))
               (setf temp-type (numeric-type-of val)
                     type-spec temp-type))
             (setf start-form (coerce start-form temp-type))
             (when (numberp by-form)
               (setf by-form (coerce by-form temp-type))))))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the bindings.

(defmethod initial-bindings ((clause for-as-arithmetic))
  (nconc (mapcan (lambda (name)
                   (ecase name
                     (:from `((,(next-var clause) ,(start-form clause))))
                     (:to (when (and (end-form clause)
                                     (not (numberp (end-form clause))))
                            `((,(end-var clause) ,(end-form clause)))))
                     (:by (unless (numberp (by-form clause))
                            `((,(by-var clause) ,(by-form clause)))))))
                 (order clause))
         (d-spec-outer-bindings (var clause))))

(defmethod initial-declarations ((clause for-as-arithmetic))
  (nconc (unless (eq (type-spec (var clause)) t)
           `((cl:type ,(temp-type clause)
                      ,(next-var clause)
                      ,@(unless (numberp (by-form clause))
                          (cl:list (by-var clause))))))
         (when (and (end-form clause)
                    (not (numberp (end-form clause))))
           `((cl:type number ,(end-var clause))))
         (d-spec-outer-declarations (var clause))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the initial-step-form.

(defmethod initial-step-forms ((clause for-as-arithmetic-up))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(next-var clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (end-var clause)))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(next-var clause))))))

(defmethod initial-step-forms ((clause for-as-arithmetic-down))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (end-var clause))
                      ,(next-var clause))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(next-var clause))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute the subsequent-step-forms.

(defmethod subsequent-step-forms ((clause for-as-arithmetic-up))
  (nconc `((incf ,(next-var clause) ,(if (numberp (by-form clause))
                                         (by-form clause)
                                         (by-var clause))))
         (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(next-var clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (end-var clause)))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(next-var clause))))))

(defmethod subsequent-step-forms ((clause for-as-arithmetic-down))
  (nconc `((decf ,(next-var clause) ,(if (numberp (by-form clause))
                                         (by-form clause)
                                         (by-var clause))))
         (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (end-var clause))
                      ,(next-var clause))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(next-var clause))))))
