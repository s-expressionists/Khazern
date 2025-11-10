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

(defclass accumulation-clause (selectable-clause var-mixin form-mixin)
  ((%reference :accessor reference
               :initarg :reference
               :initform nil)
   (%args :accessor args
          :initform nil)))

(defmethod parse-preposition
    ((client standard-client) (instance accumulation-clause) name)
  (setf (args instance) (nconc (args instance) (list name (parse-token)))))

(defmethod body-forms ((clause accumulation-clause))
  `((,(scope-reference (get-scope (var-spec (var clause))) (reference clause))
     ,(it-form (form clause))
     ,@(args clause))))

(defclass scope (clause var-mixin)
  ((%references :accessor references
                :initform nil)
   (%function-definitions :accessor function-definitions
                          :initform nil)
   (%function-declarations :accessor function-declarations
                           :initform nil)))

(defmethod make-scope :around
    ((client standard-client) name type category references)
  (declare (ignore name type category))
  (let ((instance (call-next-method)))
    (when (typep instance 'scope)
      (prog ((symbol nil))
       next
         (when references
           (setf symbol (or (second references)
                            (getf (references instance) (first references))
                            (gensym (symbol-name (first references))))
                 (getf (references instance) (first references)) symbol)
           (multiple-value-bind (definitions declarations)
               (scope-functions client instance (first references) symbol)
             (setf (function-definitions instance) (nconc (function-definitions instance)
                                                          definitions)
                   (function-declarations instance) (nconc (function-declarations instance)
                                                       declarations)
                   references (cddr references))
             (go next)))))
    instance))

(defmethod scope-reference ((instance scope) ref)
  (or (getf (references instance) ref)
      (setf (getf (references instance) ref) (gensym (symbol-name ref)))))

(defmethod wrap-forms ((instance scope) forms)
  (wrap-labels (function-definitions instance)
               (function-declarations instance)
               forms))

;;; COLLECT/APPEND/NCONC clause

(defclass sequence-scope (scope) ())

(defclass list-scope (sequence-scope) ())

(defmethod initialize-instance :after ((instance list-scope) &rest initargs &key)
  (declare (ignore initargs))
  (let ((head (add-simple-binding instance
                                  :var "HEAD" :type 'cons
                                  :form '(cons nil nil)
                                  :dynamic-extent t)))
    (setf (getf (references instance) :head) head
          (getf (references instance) :tail) (add-simple-binding instance
                                                                 :var "TAIL" :type 'cons
                                                                 :form head))))

(defmethod make-scope
    ((client standard-client) name type (category (eql :sequence)) references)
  (declare (ignore references))
  (check-subtype type 'list)
  (make-instance 'list-scope
                 :var (make-instance 'simple-binding
                                     :var-spec name
                                     :type-spec type
                                     :category category)))

(defmethod scope-functions
    ((client standard-client) (instance list-scope) (reference (eql :collect)) name)
  (let ((tail (scope-reference instance :tail)))
    (with-gensyms (value)
      (values `((,name (,value)
                  (rplacd ,tail
                          (setq ,tail (cons ,value nil)))))
              `((inline ,name))))))

(defmethod scope-functions
    ((client standard-client) (instance list-scope) (reference (eql :append)) name)
  (let ((tail (scope-reference instance :tail)))
    (with-gensyms (value repeat)
      `((,name (,value)
          (tagbody
           ,repeat
             (cond ((consp ,value)
                    (rplacd ,tail
                            (setq ,tail (cons (car ,value) nil)))
                    (setq ,value (cdr ,value))
                    (go ,repeat))
                   (t
                    (rplacd ,tail ,value)))))))))

(defmethod scope-functions
    ((client standard-client) (instance list-scope) (reference (eql :nconc)) name)
  (let ((tail (scope-reference instance :tail)))
    (with-gensyms (value repeat)
      `((,name (,value)
          (tagbody
             (rplacd ,tail ,value)
           ,repeat
             (when (consp (cdr ,tail))
               (setq ,tail (cdr ,tail))
               (go ,repeat))))))))

(defmethod wrap-forms :around ((instance list-scope) forms)
  (declare (ignore forms))
  (let ((into (var-spec (var instance)))
        (head (getf (references instance) :head)))
    `((symbol-macrolet ((,into (cdr ,head)))
        ,@(call-next-method)))))

(defun accumulate-form (var name form)
  (let ((tail (scope-reference var :tail)))
    (flet ((append-cons (a d)
             `((rplacd ,tail (setq ,tail (cons ,a ,d)))))
           (accumulate-func ()
             `((,(scope-reference var name) ,form)))
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

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :collect)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :collect)
                      :default-type-spec 'list
                      :category :sequence
                      :scope-references '(:collect nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :collecting)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :collect)
                      :default-type-spec 'list
                      :category :sequence
                      :scope-references '(:collect nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :append)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :append)
                      :default-type-spec 'list
                      :category :sequence
                      :scope-references '(:append nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :appending)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :append)
                      :default-type-spec 'list
                      :category :sequence
                      :scope-references '(:append nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :nconc)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :nconc)
                      :default-type-spec 'list
                      :category :sequence
                      :scope-references '(:nconc nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :nconcing)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :nconc)
                      :default-type-spec 'list
                      :category :sequence
                      :scope-references '(:nconc nil)))

#+(or)(defmethod body-forms ((clause list-accumulation-clause))
  `((,(scope-reference  (var-spec (var clause)) (reference clause))
     ,(it-form (form clause)))))

;;; COUNT/SUM clause

(defclass summation-scope (scope)
  ())

(defmethod make-scope
    ((client standard-client) name type (category (eql :summation)) references)
  (declare (ignore references))
  (check-subtype type 'number)
  (let ((instance (make-instance 'summation-scope)))
    (setf (var instance)
          (nth-value 1
                     (add-simple-binding instance :var name
                                                  :type (if (eq type 'complex)
                                                            'number
                                                            type)
                                                  :form (coerce 0 type)
                                                  :category category)))
    instance))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :count)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :count)
                      :default-type-spec 'fixnum
                      :parse-type-spec t
                      :category :summation
                      :scope-references '(:count nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :counting)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :count)
                      :default-type-spec 'fixnum
                      :parse-type-spec t
                      :category :summation
                      :scope-references '(:count nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :sum)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :sum)
                      :default-type-spec 'number
                      :parse-type-spec t
                      :category :summation
                      :scope-references '(:sum nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :summing)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :sum)
                      :default-type-spec 'number
                      :parse-type-spec t
                      :category :summation
                      :scope-references '(:sum nil)))

(defmethod scope-functions
    ((client standard-client) (instance summation-scope) (reference (eql :count))
     name)
  (let ((var (var-spec (var instance))))
    (with-gensyms (value)
      (values `((,name (,value)
                       (when ,value
                         (incf ,var))))
              `((inline ,name))))))

(defmethod scope-functions
    ((client standard-client) (instance summation-scope) (reference (eql :sum))
     name)
  (let ((var (var-spec (var instance))))
    (with-gensyms (value)
      (values `((,name (,value)
                       (incf ,var ,value)))
              `((inline ,name))))))

;;; MINIMIZE/MAXIMIZE clause

(defclass extremum-scope (scope) ())

(defmethod initialize-instance :after
    ((instance extremum-scope) &rest initargs &key)
  (declare (ignore initargs))
  (setf (references instance) (list :firstp
                                    (add-simple-binding instance :var "FIRSTP" :type 'boolean
                                                                 :form t))))

(defmethod make-scope
    ((client standard-client) name type (category (eql :extremum)) references)
  (declare (ignore references))
  (check-subtype type 'real)
  (let ((instance (make-instance 'extremum-scope)))
    (setf (var instance)
          (nth-value 1
                     (add-simple-binding instance :var name
                                                  :type type
                                                  :form (coerce 0 type)
                                                  :category category
                                                  :ignorable t)))
    instance))

(defmethod scope-functions
    ((client standard-client) (instance extremum-scope) (reference (eql :max))
     name)
  (let ((var (var-spec (var instance)))
        (type (type-spec (var instance)))
        (firstp (scope-reference instance :firstp)))
    (with-gensyms (value coerced-value)
      `((,name (,value)
               (let ((,coerced-value (coerce ,value ',type)))
                 (declare (type ,type ,coerced-value))
                 (when (or ,firstp
                           (> ,coerced-value ,var))
                   (setq ,var ,coerced-value
                         ,firstp nil))))))))

(defmethod scope-functions
    ((client standard-client) (instance extremum-scope) (reference (eql :min)) name)
  (let ((var (var-spec (var instance)))
        (type (type-spec (var instance)))
        (firstp (scope-reference instance :firstp)))
    (with-gensyms (value coerced-value)
      `((,name (,value)
               (let ((,coerced-value (coerce ,value ',type)))
                 (declare (type ,type ,coerced-value))
                 (when (or ,firstp
                           (< ,coerced-value ,var))
                   (setq ,var ,coerced-value
                         ,firstp nil))))))))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :minimize)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :min)
                      :default-type-spec 'real
                      :parse-type-spec t
                      :category :extremum
                      :scope-references '(:min nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :minimizing)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :min)
                      :default-type-spec 'real
                      :parse-type-spec t
                      :category :extremum
                      :scope-references '(:min nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :maximize)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :max)
                      :default-type-spec 'real
                      :parse-type-spec t
                      :category :extremum
                      :scope-references '(:max nil)))

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :maximizing)) &key)
  (parse-accumulation client
                      (make-instance 'accumulation-clause :reference :max)
                      :default-type-spec 'real
                      :parse-type-spec t
                      :category :extremum
                      :scope-references '(:max nil)))
