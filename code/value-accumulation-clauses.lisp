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
  ((%reference :accessor reference
               :initarg :reference
               :initform nil)))

(defun parse-accumulation-clause (category reference &optional default-type-spec)
  (make-instance 'accumulation-clause
                 :start *start*
                 :reference reference
                 :form (parse-token)
                 :accum-var (parse-into :default-type-spec default-type-spec                         
                                        :accumulation-category category
                                        :accumulation-references (list reference nil))
                 :end *index*))

(defmethod body-forms ((clause accumulation-clause))
  `((,(accumulation-reference (var-spec (accum-var clause)) (reference clause))
     ,(it-form (form clause)))))

(defclass accumulation-scope (clause)
  ((%accum-ref :accessor accum-ref
               :initarg :accum-ref
               :initform nil)
   (%accum-var :accessor accum-var
               :initarg :accum-var
               :initform nil)
   (%references :accessor references
                :initform nil)
   (%function-definitions :accessor function-definitions
                          :initform nil)
   (%function-declarations :accessor function-declarations
                           :initform nil)))

(defmethod make-accumulation-scope :around
    ((client standard-client) name type category references)
  (declare (ignore type references))
  (let ((instance (call-next-method)))
    (when (typep instance 'accumulation-scope)
      (prog ((symbol nil)
             (other-references (copy-list (references instance))))
       next
         (when references
           (setf symbol (or (second references)
                            (gensym (symbol-name (first references))))
                 (getf (references instance) (first references)) symbol)
           (multiple-value-bind (definitions declarations)
               (apply #'accumulation-scope-functions
                      client name type category
                      (first references)
                      symbol
                      other-references)
             (setf (function-definitions instance) (nconc (function-definitions instance)
                                                          definitions)
                   (function-declarations instance) (nconc (function-declarations instance)
                                                       declarations)
                   references (cddr references))
             (go next)))))
    instance))

(defmethod accumulation-scope-reference ((instance accumulation-scope) name ref)
  (when (eq name (accum-ref instance))
    (or (getf (references instance) ref)
        (setf (getf (references instance) ref) (gensym (symbol-name ref))))))

(defmethod wrap-forms ((instance accumulation-scope) forms)
  (wrap-flet (function-definitions instance)
             (function-declarations instance)
             forms))

;;; COLLECT/APPEND/NCONC clause

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

(defmethod make-accumulation-scope
    ((client standard-client) name type (category (eql :list)) references)
  (declare (ignore type references))
  (make-instance 'list-accumulation-scope :accum-ref name))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :list)) (reference (eql :collect)) symbol
     &key tail &allow-other-keys)
  (declare (ignore name type))
  (values `((,symbol (value)
                     (rplacd ,tail
                             (setq ,tail (cons value nil)))))
          `((inline ,symbol))))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :list)) (reference (eql :append)) symbol
     &key tail &allow-other-keys)
  `((,symbol (value)
      (tagbody
       repeat
         (cond ((consp value)
                (rplacd ,tail
                        (setq ,tail (cons (car value) nil)))
                (setq value (cdr value))
                (go repeat))
               (t
                (rplacd ,tail value)))))))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :list)) (reference (eql :nconc)) symbol
     &key tail &allow-other-keys)
  `((,symbol (value)
      (tagbody
         (rplacd ,tail value)
       repeat
         (when (consp (cdr ,tail))
           (setq ,tail (cdr ,tail))
           (go repeat))))))

(defmethod wrap-forms :around ((instance list-accumulation-scope) forms)
  (declare (ignore forms))
  (let ((into (accum-ref instance))
        (head (getf (references instance) :head)))
    `((symbol-macrolet ((,into (cdr ,head)))
        ,@(call-next-method)))))

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

(defclass list-accumulation-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :collect)) &key)
  (change-class (parse-accumulation-clause :list :collect) 'list-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :collecting)) &key)
  (change-class (parse-accumulation-clause :list :collect) 'list-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :append)) &key)
  (change-class (parse-accumulation-clause :list :append) 'list-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :appending)) &key)
  (change-class (parse-accumulation-clause :list :append) 'list-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :nconc)) &key)
  (change-class (parse-accumulation-clause :list :nconc) 'list-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :nconcing)) &key)
  (change-class (parse-accumulation-clause :list :nconc) 'list-accumulation-clause))

(defmethod body-forms ((clause list-accumulation-clause))
  `((,(accumulation-reference  (var-spec (accum-var clause)) (reference clause))
     ,(it-form (form clause)))))

;;; COUNT/SUM clause

(defclass summation-accumulation-scope (accumulation-scope)
  ())

(defmethod make-accumulation-scope
    ((client standard-client) name type (category (eql :summation)) references)
  (declare (ignore references))
  (let ((instance (make-instance 'summation-accumulation-scope)))
    (setf (values (accum-ref instance) (accum-var instance))
          (add-simple-binding instance :var name
                                       :type (if (eq type 'complex)
                                                 'number
                                                 type)
                                       :form (coerce 0 type)
                                       :accumulation-category category))
    instance))

(defclass summation-accumulation-clause (accumulation-clause)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :count)) &key)
  (change-class (parse-accumulation-clause :summation :count 'fixnum)
                'summation-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :counting)) &key)
  (change-class (parse-accumulation-clause :summation :count 'fixnum)
                'summation-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :sum)) &key)
  (change-class (parse-accumulation-clause :summation :sum 'number)
                'summation-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :summing)) &key)
  (change-class (parse-accumulation-clause :summation :sum 'number)
                'summation-accumulation-clause))

(defmethod analyze ((client standard-client) (clause summation-accumulation-clause))
  (check-subtype (type-spec (accum-var clause)) 'number))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :summation)) (reference (eql :count))
     symbol &key &allow-other-keys)
  (values `((,symbol (value)
              (when value
                (incf ,name))))
          `((inline ,symbol))))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :summation)) (reference (eql :sum))
     symbol &key &allow-other-keys)
  (values `((,symbol (value)
              (incf ,name value)))
          `((inline ,symbol))))

;;; MINIMIZE/MAXIMIZE clause

(defclass extremum-accumulation-scope (accumulation-scope) ())

(defmethod initialize-instance :after
    ((instance extremum-accumulation-scope) &rest initargs &key)
  (declare (ignore initargs))
  (setf (references instance) (list :firstp
                                    (add-simple-binding instance :var "FIRSTP" :type 'boolean
                                                                 :form t))))

(defmethod make-accumulation-scope
    ((client standard-client) name type (category (eql :extremum)) references)
  (declare (ignore references))
  (let ((instance (make-instance 'extremum-accumulation-scope)))
    (setf (values (accum-ref instance) (accum-var instance))
          (add-simple-binding instance :var name
                                       :type type
                                       :form (coerce 0 type)
                                       :accumulation-category category
                                       :ignorable t))
    instance))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :extremum)) (reference (eql :max)) symbol
     &key firstp &allow-other-keys)
  `((,symbol (value)
      (let ((coerced-value (coerce value ',type)))
          (declare (type ,type coerced-value))
          (when (or ,firstp
                    (> coerced-value ,name))
            (setq ,name coerced-value
                  ,firstp nil))))))

(defmethod accumulation-scope-functions
    ((client standard-client) name type (category (eql :extremum)) (reference (eql :min)) symbol
     &key firstp &allow-other-keys)
  `((,symbol (value)
      (let ((coerced-value (coerce value ',type)))
        (declare (type ,type coerced-value))
        (when (or ,firstp
                  (< coerced-value ,name))
          (setq ,name coerced-value
                ,firstp nil))))))

(defclass extremum-accumulation-clause (accumulation-clause) ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :minimize)) &key)
  (change-class (parse-accumulation-clause :extremum :min 'real)
                'extremum-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :minimizing)) &key)
  (change-class (parse-accumulation-clause :extremum :min 'real)
                'extremum-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :maximize)) &key)
  (change-class (parse-accumulation-clause :extremum :max 'real)
                'extremum-accumulation-clause))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :maximizing)) &key)
  (change-class (parse-accumulation-clause :extremum :max 'real)
                'extremum-accumulation-clause))

(defmethod analyze ((client standard-client) (clause extremum-accumulation-clause))
  (check-subtype (type-spec (accum-var clause)) 'real))
