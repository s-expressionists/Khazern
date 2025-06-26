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

(defun parse-into ()
  (if (pop-token? :keywords '(:into))
      (pop-token :type 'symbol)
      (default-accumulation-variable)))

(defclass list-accumulation-clause (clause)
  ((%head :accessor head)
   (%tail :accessor tail)
   (%accum :accessor accum)
   (%append-func :accessor append-func
                 :initform nil)
   (%nconc-func :accessor nconc-func
                :initform nil)))

(defmethod initialize-instance :after ((instance list-accumulation-clause) &rest initargs &key)
  (declare (ignore initargs))
  (setf (head instance) (add-simple-binding instance :var "HEAD" :type 'cons
                                                     :form '(cons nil nil) :dynamic-extent t)
        (tail instance) (add-simple-binding instance :var "TAIL" :type 'cons
                                                     :form (head instance))))

(defmethod accumulation-clause-reference
    ((instance list-accumulation-clause) name (ref (eql :tail)))
  (and (eq name (accum instance))
       (tail instance)))
                                       
(defmethod accumulation-clause-reference
    ((instance list-accumulation-clause) name (ref (eql :append)))
  (cond ((not (eq name (accum instance)))
         nil)
        ((null (append-func instance))
         (setf (append-func instance) (gensym "APPEND")))
        (t
         (append-func instance))))

(defmethod accumulation-clause-reference
    ((instance list-accumulation-clause) name (ref (eql :nconc)))
  (cond ((not (eq name (accum instance)))
         nil)
        ((null (nconc-func instance))
         (setf (nconc-func instance) (gensym "NCONC")))
        (t
         (nconc-func instance))))

(defmethod make-accumulation-clause (name type (category (eql :list)))
  (let ((instance (make-instance 'list-accumulation-clause)))
    (setf (accum instance) (add-simple-binding instance :var name
                                                        :type type
                                                        :accumulation-category category
                                                        :ignorable t))
    instance))

(defmethod wrap-forms ((instance list-accumulation-clause) forms)
  (let ((head (head instance))
        (tail (tail instance))
        (into-var (accum instance)))
    (with-accessors ((append-func append-func)
                     (nconc-func nconc-func))
        instance
      `((symbol-macrolet ((,into-var (cdr ,head)))
          ,@(if (or append-func nconc-func)
                `((flet (,@(when append-func
                             `((,append-func (value)
                                 (tagbody
                                  repeat
                                    (cond ((consp value)
                                           (rplacd ,tail
                                                   (setq ,tail (cons (car value) nil)))
                                           (setq value (cdr value))
                                           (go repeat))
                                          (t
                                           (rplacd ,tail value)))))))
                         ,@(when nconc-func
                             `((,nconc-func (value)
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

(defclass summation-accumulation-clause (clause)
  ())

(defmethod make-accumulation-clause (name type (category (eql :summation)))
  (let ((instance (make-instance 'summation-accumulation-clause)))
    (add-simple-binding instance :var name
                                 :type (if (eq type 'complex)
                                           'number
                                           type)
                                 :form (coerce 0 type)
                                 :accumulation-category category)
    instance))

(defclass extremum-accumulation-clause (clause)
  ((%first-var :accessor first-var)
   (%accum-var :accessor accum-var)
   (%max-func :accessor max-func
              :initform nil)
   (%min-func :accessor min-func
              :initform nil)))

(defmethod initialize-instance :after ((instance extremum-accumulation-clause) &rest initargs &key)
  (declare (ignore initargs))
  (setf (first-var instance) (add-simple-binding instance :var "FIRST" :type 'boolean :form t)))

(defmethod make-accumulation-clause (name type (category (eql :extremum)))
  (let ((instance (make-instance 'extremum-accumulation-clause)))
    (setf (accum-var instance)
          (nth-value 1 (add-simple-binding instance :var name
                                                    :type type
                                                    :form (coerce 0 type)
                                                    :accumulation-category category
                                                    :ignorable t)))
    instance))

(defmethod accumulation-clause-reference
    ((instance extremum-accumulation-clause) name (ref (eql :max)))
  (cond ((not (eq name (var-spec (accum-var instance))))
         nil)
        ((null (max-func instance))
         (setf (max-func instance) (gensym "MAX")))
        (t
         (max-func instance))))

(defmethod accumulation-clause-reference
    ((instance extremum-accumulation-clause) name (ref (eql :min)))
  (cond ((not (eq name (var-spec (accum-var instance))))
         nil)
        ((null (min-func instance))
         (setf (min-func instance) (gensym "MIN")))
        (t
         (min-func instance))))

(defmethod wrap-forms ((instance extremum-accumulation-clause) forms)
  (let ((first-var (first-var instance))
        (into-var (var-spec (accum-var instance)))
        (into-type (type-spec (accum-var instance))))
    (with-accessors ((max-func max-func)
                     (min-func min-func))
        instance
      `((flet (,@(when max-func
                   `((,max-func (value)
                       (let ((coerced-value (coerce value ',into-type)))
                         (declare (type ,into-type coerced-value))
                         (when (or ,first-var
                                   (> coerced-value ,into-var))
                           (setq ,into-var coerced-value
                                 ,first-var nil))))))
               ,@(when min-func
                   `((,min-func (value)
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
    ((client standard-client) (scope selectable-superclause) (keyword (eql :collect)))
  (make-instance 'collect-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :accumulation-category :list)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :collecting)))
  (make-instance 'collect-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :accumulation-category :list)
                 :end *index*))

(defmethod body-forms ((clause collect-clause))
  (let ((tail (accumulation-reference (var-spec (accum-var clause)) :tail)))
    `((rplacd ,tail
              (setq ,tail (cons ,(it-form (form clause)) nil))))))

;;; APPEND clause

(defclass append-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :append)))
  (make-instance 'append-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :accumulation-category :list)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :appending)))
  (make-instance 'append-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :accumulation-category :list)
                 :end *index*))

(defmethod body-forms ((clause append-clause))
  (accumulate-form (var-spec (accum-var clause)) :append (it-form (form clause))))

;;; NCONC clause

(defclass nconc-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :nconc)))
  (make-instance 'nconc-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :accumulation-category :list)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :nconcing)))
  (make-instance 'nconc-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :accumulation-category :list)
                 :end *index*))

(defmethod body-forms ((clause nconc-clause))
  (accumulate-form (var-spec (accum-var clause)) :nconc (it-form (form clause))))

;;; COUNT clause

(defclass count-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :count)))
  (make-instance 'count-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'fixnum)                          
                                           :accumulation-category :summation)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :counting)))
  (make-instance 'count-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'fixnum)                          
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
    ((client standard-client) (scope selectable-superclause) (keyword (eql :minimize)))
  (make-instance 'extremum-clause
                 :start *start*
                 :form (pop-token)
                 :func-name :min
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'real)                          
                                           :accumulation-category :extremum)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :minimizing)))
  (make-instance 'extremum-clause
                 :start *start*
                 :form (pop-token)
                 :func-name :min
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'real)                          
                                           :accumulation-category :extremum)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :maximize)))
  (make-instance 'extremum-clause
                 :start *start*
                 :form (pop-token)
                 :func-name :max
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'real)                          
                                           :accumulation-category :extremum)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :maximizing)))
  (make-instance 'extremum-clause
                 :start *start*
                 :form (pop-token)
                 :func-name :max
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'real)                          
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
    ((client standard-client) (scope selectable-superclause) (keyword (eql :sum)))
  (make-instance 'sum-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'number)                          
                                           :accumulation-category :summation)
                 :end *index*))

(defmethod parse-clause
    ((client standard-client) (scope selectable-superclause) (keyword (eql :summing)))
  (make-instance 'sum-clause
                 :start *start*
                 :form (pop-token)
                 :accum-var (make-instance 'simple-binding
                                           :var-spec (parse-into)
                                           :type-spec (parse-type-spec 'number)                          
                                           :accumulation-category :summation)
                 :end *index*))

(defmethod analyze ((clause sum-clause))
  (check-subtype (type-spec (accum-var clause)) 'number))

(defmethod body-forms ((clause sum-clause))
  `((incf ,(var-spec (accum-var clause))
          ,(it-form (form clause)))))
