(cl:in-package #:khazern)

;;; 6.1.3 Value Accumulation Clauses

(defclass accumulation-clause (selectable-clause accumulation-mixin form-mixin)
  ())

(defun parse-into (tokens)
  (if (pop-token? tokens :keywords '(:into))
      (pop-token tokens :type 'symbol)
      (default-accumulation-variable)))

(defclass list-accumulation-clause (var-mixin)
  ((%head-var :reader head-var
              :initform (make-instance 'd-spec
                                       :var-spec (gensym "HEAD")
                                       :type-spec 'cons))
   (%tail-var :reader tail-var
              :initform (make-instance 'd-spec
                                       :var-spec (gensym "TAIL")
                                       :type-spec 'cons))
   (%append-func :accessor append-func
                 :initform nil)
   (%nconc-func :accessor nconc-func
                :initform nil)))

(defmethod accumulation-clause-reference ((instance list-accumulation-clause) name (ref (eql :tail)))
  (and (eq name (var-spec (var instance)))
       (var-spec (tail-var instance))))
                                       
(defmethod accumulation-clause-reference ((instance list-accumulation-clause) name (ref (eql :append)))
  (cond ((not (eq name (var-spec (var instance))))
         nil)
        ((null (append-func instance))
         (setf (append-func instance) (gensym "APPEND")))
        (t
         (append-func instance))))

(defmethod accumulation-clause-reference ((instance list-accumulation-clause) name (ref (eql :nconc)))
  (cond ((not (eq name (var-spec (var instance))))
         nil)
        ((null (nconc-func instance))
         (setf (nconc-func instance) (gensym "NCONC")))
        (t
         (nconc-func instance))))

(defmethod make-accumulation-clause (name type (category (eql :list)))
  (make-instance 'list-accumulation-clause
                 :var (make-instance 'd-spec
                                     :var-spec name
                                     :type-spec type
                                     :accumulation-category category)))

(defmethod initial-bindings ((instance list-accumulation-clause))
  (nconc (d-spec-simple-bindings (head-var instance) '(cons nil nil))
         (d-spec-simple-bindings (tail-var instance) (var-spec (head-var instance)))))

(defmethod initial-declarations ((instance list-accumulation-clause))
  (nconc `((dynamic-extent ,(var-spec (head-var instance))))
         (d-spec-outer-declarations (head-var instance))
         (d-spec-outer-declarations (tail-var instance))))

(defmethod wrap-forms ((instance list-accumulation-clause) forms)
  (let ((head-var (var-spec (head-var instance)))
        (tail-var (var-spec (tail-var instance)))
        (into-var (var-spec (var instance))))
    (with-accessors ((append-func append-func)
                     (nconc-func nconc-func))
        instance
      `((symbol-macrolet ((,into-var (cdr ,head-var)))
          ,@(if (or append-func nconc-func)
                `((flet (,@(when append-func
                             `((,append-func (value)
                                 (tagbody
                                  repeat
                                    (cond ((consp value)
                                           (rplacd ,tail-var
                                                   (setq ,tail-var (cons (car value) nil)))
                                           (setq value (cdr value))
                                           (go repeat))
                                          (t
                                           (rplacd ,tail-var value)))))))
                         ,@(when nconc-func
                             `((,nconc-func (value)
                                 (tagbody
                                    (rplacd ,tail-var value)
                                  repeat
                                    (when (consp (cdr ,tail-var))
                                      (setq ,tail-var (cdr ,tail-var))
                                      (go repeat)))))))
                    ,@forms))
                forms))))))

(defclass summation-accumulation-clause (var-mixin)
  ())

(defmethod make-accumulation-clause (name type (category (eql :summation)))
  (make-instance 'summation-accumulation-clause
                 :var (make-instance 'd-spec
                                     :var-spec name
                                     :type-spec (if (eq type 'complex)
                                                    'number
                                                    type)
                                     :accumulation-category category)))

(defmethod initial-bindings ((instance summation-accumulation-clause))
  (d-spec-outer-bindings (var instance)))

(defmethod initial-declarations ((instance summation-accumulation-clause))
  (d-spec-outer-declarations (var instance)))

(defclass extremum-accumulation-clause (var-mixin)
  ())

(defmethod make-accumulation-clause (name type (category (eql :extremum)))
  (make-instance 'extremum-accumulation-clause
                 :var (make-instance 'd-spec
                                     :var-spec name
                                     :type-spec type
                                     :accumulation-category category)))

(defmethod initial-bindings ((instance extremum-accumulation-clause))
  (d-spec-outer-bindings (var instance)))

(defmethod initial-declarations ((instance extremum-accumulation-clause))
  (d-spec-outer-declarations (var instance)))

;;; COLLECT clause

(defclass collect-clause (accumulation-clause)
  ())

;;; COLLECT parsers

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :collect)) tokens)
  (make-instance 'collect-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :accumulation-category :list)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :collecting)) tokens)
  (make-instance 'collect-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :accumulation-category :list)))

;;; COLLECT expansion methods

(defmethod body-forms ((clause collect-clause))
  (let ((tail-var (accumulation-reference (var-spec (var clause)) :tail)))
    `((rplacd ,tail-var
              (setq ,tail-var (cons ,(it-form (form clause)) nil))))))

;;; APPEND clause

(defclass append-clause (accumulation-clause)
  ())

;;; APPEND parsers

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :append)) tokens)
  (make-instance 'append-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :accumulation-category :list)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :appending)) tokens)
  (make-instance 'append-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :accumulation-category :list)))

;;; APPEND expansion methods

(defmethod body-forms ((clause append-clause))
  `((,(accumulation-reference (var-spec (var clause)) :append) ,(it-form (form clause)))))

;;; NCONC clause

(defclass nconc-clause (accumulation-clause)
  ())

;;; NCONC parsers

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :nconc)) tokens)
  (make-instance 'nconc-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :accumulation-category :list)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :nconcing)) tokens)
  (make-instance 'nconc-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :accumulation-category :list)))

;;; NCONC expansion methods

(defmethod body-forms ((clause nconc-clause))
  `((,(accumulation-reference (var-spec (var clause)) :nconc) ,(it-form (form clause)))))

;;; COUNT clause

(defclass count-clause (accumulation-clause)
  ())

;;; COUNT parsers

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :count)) tokens)
  (make-instance 'count-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'fixnum)                          
                                     :accumulation-category :summation)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :counting)) tokens)
  (make-instance 'count-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'fixnum)                          
                                     :accumulation-category :summation)))

;;; COUNT expansion methods

(defmethod analyze ((clause count-clause))
  (check-subtype (type-spec (var clause)) 'number))

(defmethod body-forms ((clause count-clause))
  (let ((form (form clause))
        (into-var (var-spec (var clause))))
    `((when ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)
        (setq ,into-var
              (1+ ,into-var))))))

;;; MINIMIZE/MAXIMIZE clause

(defclass extremum-clause (accumulation-clause)
  ((%reduce-function :reader reduce-function
                     :initarg :reduce-function)))

;;; MINIMIZE/MAXIMIZE parsers

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :minimize)) tokens)
  (make-instance 'extremum-clause
                 :form (pop-token tokens)
                 :reduce-function 'min
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'real)                          
                                     :accumulation-category :extremum)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :minimizing)) tokens)
  (make-instance 'extremum-clause
                 :form (pop-token tokens)
                 :reduce-function 'min
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'real)                          
                                     :accumulation-category :extremum)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :maximize)) tokens)
  (make-instance 'extremum-clause
                 :form (pop-token tokens)
                 :reduce-function 'max
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'real)                          
                                     :accumulation-category :extremum)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :maximizing)) tokens)
  (make-instance 'extremum-clause
                 :form (pop-token tokens)
                 :reduce-function 'max
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'real)                          
                                     :accumulation-category :extremum)))

;;; MINIMIZE/MAXIMIZE expansion methods

(defmethod analyze ((clause extremum-clause))
  (setf (type-spec (var clause)) (type-or-null (type-spec (var clause))))
  (check-subtype (type-spec (var clause)) '(or null real)))

(defmethod body-forms ((clause extremum-clause))
  (let ((form (form clause))
        (var (var-spec (var clause))))
    (when (and *it-var* (it-keyword-p form))
      (setf form *it-var*))
    `((setq ,var
            (if ,var
                (,(reduce-function clause) ,var ,form)
                ,form)))))

;;; SUM clause

(defclass sum-clause (accumulation-clause)
  ())

;;; SUM parsers

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :sum)) tokens)
  (make-instance 'sum-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'number)                          
                                     :accumulation-category :summation)))

(defmethod parse-clause ((client standard-client) (scope selectable-superclause) (keyword (eql :summing)) tokens)
  (make-instance 'sum-clause
                 :form (pop-token tokens)
                 :var (make-instance 'd-spec
                                     :var-spec (parse-into tokens)
                                     :type-spec (parse-type-spec tokens 'number)                          
                                     :accumulation-category :summation)))

;;; SUM expansion methods

(defmethod analyze ((clause sum-clause))
  (check-subtype (type-spec (var clause)) 'number))

(defmethod body-forms ((clause sum-clause))
  (let ((form (form clause)))
    `((incf ,(var-spec (var clause))
            ,(if (and *it-var* (it-keyword-p form))
                 *it-var*
                 form)))))
