(cl:in-package #:khazern)

;;; 6.1.3 Value Accumulation Clauses
;;;
;;; Syntax:
;;;
;;;   accumulation ::= list-accumulation | numeric-accumulation 
;;;   list-accumulation ::= {COLLECT | COLLECTING | APPEND | APPENDING | NCONC | NCONCING}
;;;                         {form | IT} [INTO simple-var] 
;;;   numeric-accumulation::= {COUNT | COUNTING | SUM | SUMMING | MAXIMIZE | MAXIMIZING |
;;;                            MINIMIZE | MINIMIZING} {form | IT} [INTO simple-var] [type-spec]

(defclass accumulation-clause (selectable-clause accumulation-mixin form-mixin)
  ())

(defclass accumulation-scope (clause)
  ((%accum-ref :accessor accum-ref
               :initarg :accum-ref
               :initform nil)
   (%accum-var :accessor accum-var
               :initarg :accum-var
               :initform nil)
   (%references :accessor references
                :initform nil)))

(defmethod accumulation-scope-reference ((instance accumulation-scope) name ref)
  (when (eq name (accum-ref instance))
    (let ((val (getf (references instance) ref)))
      (or val
          (setf (getf (references instance) ref) (gensym (symbol-name name)))
         nil))))

(defclass list-accumulation-scope (accumulation-scope) ())

(defmethod initialize-instance :after ((instance list-accumulation-scope) &rest initargs &key)
  (declare (ignore initargs))
  (let ((head (add-simple-binding instance
                                  :var "HEAD" :type 'cons
                                  :form '(cons nil nil)
                                  :dynamic-extent t)))
    (setf (getf (references instance) :head) head
          (getf (references instance) :tail) (add-simple-binding instance
                                                                 :var "TAIL" :type 'cons
                                                                 :form head))))

(defmethod make-accumulation-scope (name type (category (eql :list)))
  (declare (ignore type))
  (make-instance 'list-accumulation-scope :accum-ref name))

(defmethod wrap-forms ((instance list-accumulation-scope) forms)
  (destructuring-bind (&key head tail append nconc)
      (references instance)
    (let ((into-var (accum-ref instance)))
      `((symbol-macrolet ((,into-var (cdr ,head)))
          ,@(if (or append nconc)
                `((flet (,@(when append
                             `((,append (value)
                                        (tagbody
                                         repeat
                                           (cond ((consp value)
                                                  (rplacd ,tail
                                                          (setq ,tail (cons (car value) nil)))
                                                  (setq value (cdr value))
                                                  (go repeat))
                                                 (t
                                                  (rplacd ,tail value)))))))
                         ,@(when nconc
                             `((,nconc (value)
                                       (tagbody
                                          (rplacd ,tail value)
                                        repeat
                                          (when (consp (cdr ,tail))
                                            (setq ,tail (cdr ,tail))
                                            (go repeat)))))))
                    ,@forms))
                forms))))))

(defun accumulate-form (var name form)
  (let ((tail (accumulation-reference var :tail)))
    (flet ((append-cons (a d)
             `((rplacd ,tail (setq ,tail (cons ,a ,d)))))
           (accumulate-func ()
             `((,(accumulation-reference var name) ,form)))
           (quote-form (form)
             (if (constantp form)
                 form
                 `(quote ,form))))
      (if (consp form)
          (case (car form)
            (quote
             (if (and (eq name :append)
                      (consp (second form)))
                 (prog ((head (second form))
                        forms)
                  next
                    (unless (consp head)
                      (return forms))
                    (setf forms
                          (nconc forms
                                 (append-cons (quote-form (car head))
                                              (unless (consp (cdr head))
                                                (quote-form (cdr head))))))
                    (pop head)
                    (go next))
                 (accumulate-func)))
            (cons
             (if (constantp (third form))
                 (append-cons (second form) (third form))
                 (nconc (append-cons (second form) nil)
                        (accumulate-form var name (third form)))))
            (list
             (mapcan (lambda (form)
                       (append-cons form nil))
                     (cdr form)))
            (list*
             (mapcon (lambda (head)
                       (cond ((null (cdr head))
                              (unless (constantp (car head))
                                (accumulate-form var name (car head))))
                             ((null (cddr head))
                              (append-cons (car head)
                                           (when (constantp (second head))
                                             (second head))))
                             (t
                              (append-cons (car head) nil))))
                     (cdr form)))
            (otherwise
             (accumulate-func)))
          (accumulate-func)))))

(defclass summation-accumulation-scope (accumulation-scope)
  ())

(defmethod make-accumulation-scope (name type (category (eql :summation)))
  (let ((instance (make-instance 'summation-accumulation-scope)))
    (setf (values (accum-ref instance) (accum-var instance))
          (add-simple-binding instance :var name
                                       :type (if (eq type 'complex)
                                                 'number
                                                 type)
                                       :form (coerce 0 type)
                                       :accumulation-category category))
    instance))

(defclass extremum-accumulation-scope (accumulation-scope)
  ((%first-var :accessor first-var)))

(defmethod initialize-instance :after
    ((instance extremum-accumulation-scope) &rest initargs &key)
  (declare (ignore initargs))
  (setf (first-var instance) (add-simple-binding instance :var "FIRST" :type 'boolean :form t)))

(defmethod make-accumulation-scope (name type (category (eql :extremum)))
  (let ((instance (make-instance 'extremum-accumulation-scope)))
    (setf (values (accum-ref instance) (accum-var instance))
          (add-simple-binding instance :var name
                                       :type type
                                       :form (coerce 0 type)
                                       :accumulation-category category
                                       :ignorable t))
    instance))

(defmethod wrap-forms ((instance extremum-accumulation-scope) forms)
  (destructuring-bind (&key max min)
      (references instance)
    (let ((first-var (first-var instance))
          (into-var (accum-ref instance))
          (into-type (type-spec (accum-var instance))))
      `((flet (,@(when max
                   `((,max (value)
                           (let ((coerced-value (coerce value ',into-type)))
                             (declare (type ,into-type coerced-value))
                             (when (or ,first-var
                                       (> coerced-value ,into-var))
                               (setq ,into-var coerced-value
                                     ,first-var nil))))))
               ,@(when min
                   `((,min (value)
                           (let ((coerced-value (coerce value ',into-type)))
                             (declare (type ,into-type coerced-value))
                             (when (or ,first-var
                                       (< coerced-value ,into-var))
                               (setq ,into-var coerced-value
                                     ,first-var nil)))))))
          ,@forms)))))
      
;;; COLLECT clause

(defclass collect-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :collect)) &key)
  (make-instance 'collect-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :accumulation-category :list)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :collecting)) &key)
  (make-instance 'collect-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :accumulation-category :list)
                 :end *index*))

(defmethod body-forms ((clause collect-clause))
  (let ((tail (accumulation-reference (var-spec (accum-var clause)) :tail)))
    `((rplacd ,tail
              (setq ,tail (cons ,(it-form (form clause)) nil))))))

;;; APPEND clause

(defclass append-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :append)) &key)
  (make-instance 'append-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :accumulation-category :list)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :appending)) &key)
  (make-instance 'append-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :accumulation-category :list)
                 :end *index*))

(defmethod body-forms ((clause append-clause))
  (accumulate-form (var-spec (accum-var clause)) :append (it-form (form clause))))

;;; NCONC clause

(defclass nconc-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :nconc)) &key)
  (make-instance 'nconc-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :accumulation-category :list)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :nconcing)) &key)
  (make-instance 'nconc-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :accumulation-category :list)
                 :end *index*))

(defmethod body-forms ((clause nconc-clause))
  (accumulate-form (var-spec (accum-var clause)) :nconc (it-form (form clause))))

;;; COUNT clause

(defclass count-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :count)) &key)
  (make-instance 'count-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :default-type-spec 'fixnum
                                        :accumulation-category :summation)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :counting)) &key)
  (make-instance 'count-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :default-type-spec 'fixnum                         
                                        :accumulation-category :summation)
                 :end *index*))

(defmethod analyze ((clause count-clause))
  (check-subtype (type-spec (accum-var clause)) 'number))

(defmethod body-forms ((clause count-clause))
  `((when ,(it-form (form clause))
      (incf ,(var-spec (accum-var clause))))))

;;; MINIMIZE/MAXIMIZE clause

(defclass extremum-clause (accumulation-clause)
  ((%func-name :reader func-name
               :initarg :func-name)))

;;; MINIMIZE/MAXIMIZE parsers

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :minimize)) &key)
  (make-instance 'extremum-clause
                 :start *start*
                 :form (parse-token)
                 :func-name :min
                 :accum-var (parse-into :default-type-spec 'real                          
                                        :accumulation-category :extremum)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :minimizing)) &key)
  (make-instance 'extremum-clause
                 :start *start*
                 :form (parse-token)
                 :func-name :min
                 :accum-var (parse-into :default-type-spec 'real                          
                                        :accumulation-category :extremum)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :maximize)) &key)
  (make-instance 'extremum-clause
                 :start *start*
                 :form (parse-token)
                 :func-name :max
                 :accum-var (parse-into :default-type-spec 'real                        
                                        :accumulation-category :extremum)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :maximizing)) &key)
  (make-instance 'extremum-clause
                 :start *start*
                 :form (parse-token)
                 :func-name :max
                 :accum-var (parse-into :default-type-spec 'real              
                                        :accumulation-category :extremum)
                 :end *index*))

(defmethod analyze ((clause extremum-clause))
  (check-subtype (type-spec (accum-var clause)) 'real))

(defmethod body-forms ((clause extremum-clause))
  `((,(accumulation-reference (var-spec (accum-var clause)) (func-name clause))
      ,(it-form (form clause)))))

;;; SUM clause

(defclass sum-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :sum)) &key)
  (make-instance 'sum-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :default-type-spec 'number                 
                                        :accumulation-category :summation)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :summing)) &key)
  (make-instance 'sum-clause
                 :start *start*
                 :form (parse-token)
                 :accum-var (parse-into :default-type-spec 'number                         
                                        :accumulation-category :summation)
                 :end *index*))

(defmethod analyze ((clause sum-clause))
  (check-subtype (type-spec (accum-var clause)) 'number))

(defmethod body-forms ((clause sum-clause))
  `((incf ,(var-spec (accum-var clause))
          ,(it-form (form clause)))))
