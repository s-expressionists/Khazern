(cl:in-package #:khazern)

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
