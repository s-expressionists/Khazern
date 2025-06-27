(cl:in-package #:khazern)

(defun nthcdr-form (n form)
  (case n
    (0 form)
    (1 `(cdr ,form))
    (2 `(cddr ,form))
    (3 `(cdddr ,form))
    (4 `(cddddr ,form))
    (otherwise
     (nthcdr-form (- n 4)
                  `(cddddr ,form)))))

(defun type-or-null (type)
  (if (typep nil type)
      type
      `(or null ,type)))

;;; Loop keywords are symbols, but they are not recognized by symbol
;;; identity as is usually the case, but instead by their names.  The
;;; HyperSpec doesn't say what function to use for comparing the
;;; names.  We assume string= here, meaning that the names are case
;;; sensitive.
(defun symbol-equal (symbol1 symbol2)
  (and (symbolp symbol1)
       (symbolp symbol2)
       (string= symbol1 symbol2)))

(defun it-keyword-p (symbol)
  (symbol-equal symbol :it))

(defun wrap-let (bindings declarations forms)
  (cond ((and bindings declarations)
         `((let ,bindings
             (declare ,@declarations)
             ,@forms)))
        (bindings
         `((let ,bindings
             ,@forms)))
        (declarations
         `((locally
               (declare ,@declarations)
             ,@forms)))
        (t
         forms)))

(defun wrap-let* (bindings declarations forms)
  (cond ((and bindings declarations)
         `((let* ,bindings
             (declare ,@declarations)
             ,@forms)))
        (bindings
         `((let* ,bindings
             ,@forms)))
        (declarations
         `((locally
               (declare ,@declarations)
             ,@forms)))
        (t
         forms)))

;;; This is very hacky
(defun numeric-types ()
  (let ((types (list 'complex 'number)))
    (mapc (lambda (type)
            (pushnew (type-of (coerce 1 type)) types :test #'equalp))
          '((complex long-float)
            (complex double-float)
            (complex single-float)
            (complex short-float)))
    (push 'real types)
    (push 'rational types)
    (mapc (lambda (type)
            (pushnew (type-of (coerce 1 type)) types :test #'equalp))
          '(long-float
            double-float
            single-float
            short-float))
    (push 'integer types)
    (push 'fixnum types)
    types))

(defvar *numeric-types* (numeric-types))

(defun numeric-type-of (value)
  (find value *numeric-types* :test #'typep))

(defun numeric-super-type (&rest types)
  (find-if (lambda (supertype)
             (every (lambda (subtype)
                      (subtypep subtype supertype))
                    types))
           *numeric-types*))

(defvar +initial-values+
  (remove-duplicates '(nil #\*
                       0 0s0 0f0 0d0 0l0
                       #C(0s0 0s0) #C(0f0 0f0) #C(0d0 0d0) #C(0l0 0l0)
                       "" #P"")))

(defun deduce-initial-value (type)
  (loop for value in +initial-values+
        when (typep value type)
          do (return-from deduce-initial-value
               (values (coerce value type)
                       t)))
  (if (consp type)
      (case (first type)
        ((integer rational short-float single-float double-float long-float float real)
         (let ((value (find-if #'numberp (cdr type))))
           (if value
               (values (coerce value type) t)
               (values nil nil))))
        (complex
         (multiple-value-bind (value validp)
             (deduce-initial-value (second type))
           (if validp
               (values (complex value value) t)
               (values nil nil))))
        (otherwise
         (values nil nil)))
      (values nil nil)))

;;; A d-var-spec is a is a destructuring variable specifier: 
;;; 
;;;    d-var-spec ::= simple-var | nil | (d-var-spec . d-var-spec)
;;;
;;; where simple-var is a symbol (a name of a variable). 
;;;

;;; Return true if and only if the argument is a valid d-var-spec, in
;;; other words if it is a tree of CONS cells where the leaves are
;;; symbols.
(defun d-var-spec-p (object)
  (or (null object)
      (and (symbolp object)
           (not (constantp object)))
      (and (consp object)
           (d-var-spec-p (car object))
           (d-var-spec-p (cdr object)))))

(defclass binding ()
  ((%var-spec :accessor var-spec
              :initarg :var-spec)
   (%type-spec :accessor type-spec
               :initarg :type-spec
               :initform t)
   (%ignorable :accessor ignorablep
               :initarg :ignorable
               :initform nil)
   (%dynamic-extent :accessor dynamic-extent-p
                    :initarg :dynamic-extent
                    :initform nil)))

(defclass simple-binding (binding)
  ((%accumulation-category :accessor accumulation-category
                           :initarg :accumulation-category
                           :initform nil)
   (%form :accessor form
          :initarg :form
          :initform nil)))

(defclass destructuring-binding (binding)
  ((%temps :accessor temps)))

(defun set-d-spec-temps (d-spec &optional temp-var-p)
  (let ((temps (make-hash-table)))
    (labels ((traverse (d-var-spec)
               (cond ((null d-var-spec)
                      nil)
                     ((symbolp d-var-spec)
                      (when temp-var-p
                        (setf (gethash d-var-spec temps) (gensym "TMP")))
                      t)
                     ((not (consp d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     (t
                      (let ((car-p (traverse (car d-var-spec)))
                            (cdr-p (traverse (cdr d-var-spec))))
                        (when (and car-p cdr-p)
                          (setf (gethash d-var-spec temps) (gensym "DE")))
                          (or car-p cdr-p))))))
      (traverse (var-spec d-spec))
      (setf (temps d-spec) temps))))

(defmethod initialize-instance :after
    ((clause destructuring-binding) &rest initargs &key ((:temp-var temp-var-p) nil))
  (declare (ignore initargs))
  (set-d-spec-temps clause temp-var-p))

(defun make-simple-binding (name
                            &key (type t) form ((:ignorable ignorablep) nil)
                                 accumulation-category
                                 ((:dynamic-extent dynamic-extent-p) nil))
  (make-instance 'simple-binding
                 :var-spec (if (symbolp name)
                               name
                               (gensym name))
                 :type-spec type
                 :form form
                 :accumulation-category accumulation-category
                 :ignorable ignorablep
                 :dynamic-extent dynamic-extent-p))

(defun make-destructuring-binding (spec
                                   &key (type t) ((:ignorable ignorablep) nil)
                                        ((:dynamic-extent dynamic-extent-p) nil))
  (make-instance 'destructuring-binding
                 :var-spec spec
                 :type-spec type
                 :ignorable ignorablep
                 :dynamic-extent dynamic-extent-p))

(defun d-spec-prep-assignments (d-spec form)
  (let ((assignments '())
        (temps (temps d-spec)))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec))
                     ((symbolp d-var-spec)
                      (let ((temp (gethash d-var-spec temps)))
                        (when temp
                          (push temp assignments)
                          (push form assignments))))
                     ((not (consp d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     (t
                      (let ((temp (gethash d-var-spec temps)))
                        (cond (temp
                               (push temp assignments)
                               (push form assignments)
                               (traverse (car d-var-spec) `(car ,temp))
                               (traverse (cdr d-var-spec) `(cdr ,temp)))
                              (t
                               (traverse (car d-var-spec) `(car ,form))
                               (traverse (cdr d-var-spec) `(cdr ,form)))))))))
      (traverse (var-spec d-spec) form)
      (when assignments
        `((setq ,.(nreverse assignments)))))))

(defun d-spec-inner-assignments (d-spec form)
  (let ((assignments '())
        (temps (temps d-spec)))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec))
                     ((symbolp d-var-spec)
                      (push d-var-spec assignments)
                      (push (or (gethash d-var-spec temps) form) assignments))
                     ((not (consp d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     (t
                      (let ((temp (gethash d-var-spec temps)))
                        (cond (temp
                               (traverse (car d-var-spec) `(car ,temp))
                               (traverse (cdr d-var-spec) `(cdr ,temp)))
                              (t
                               (traverse (car d-var-spec) `(car ,form))
                               (traverse (cdr d-var-spec) `(cdr ,form)))))))))
      (traverse (var-spec d-spec) form)
      (when assignments
        `((setq ,.(nreverse assignments)))))))

(defun d-spec-inner-form (d-spec form)
  (nconc (d-spec-prep-assignments d-spec form)
         (d-spec-inner-assignments d-spec form)))

(defmethod map-variables progn (function (d-spec binding))
  (labels ((traverse (var-spec type-spec)
             (cond ((null var-spec))
                   ((symbolp var-spec)
                    (funcall function var-spec (or type-spec t)
                             (accumulation-category d-spec)))
                   ((not (consp var-spec))
                    (error 'expected-var-spec-but-found
                           :found var-spec))
                   ((symbolp type-spec)
                    (traverse (car var-spec) type-spec)
                    (traverse (cdr var-spec) type-spec))
                   ((not (consp type-spec))
                    (error 'expected-type-spec-but-found
                           :found type-spec))
                   (t
                    (traverse (car var-spec) (car type-spec))
                    (traverse (cdr var-spec) (cdr type-spec))))
             nil))
    (traverse (var-spec d-spec) (type-spec d-spec))))

(defun check-type-spec (instance)
  (labels ((deduce (type-spec)
             (cond ((nth-value 1 (deduce-initial-value type-spec))
                    type-spec)
                   (t
                    (warn 'unable-to-deduce-initial-value
                          :type-spec type-spec)
                    `(or ,(if (subtypep type-spec 'number)
                              'bit
                              'null)
                         ,type-spec))))
           (traverse (var-spec type-spec)
             (cond ((null var-spec)
                    nil)
                   ((symbolp var-spec)
                    (deduce type-spec))
                   ((not (consp var-spec))
                    (error 'expected-var-spec-but-found
                           :found var-spec))
                   ((symbolp type-spec)
                    (setf type-spec (deduce type-spec))
                    (cons (traverse (car var-spec) type-spec)
                          (traverse (cdr var-spec) type-spec)))
                   ((not (consp type-spec))
                    (error 'expected-type-spec-but-found
                           :found type-spec))
                   (t
                    (cons (traverse (car var-spec) (car type-spec))
                          (traverse (cdr var-spec) (cdr type-spec)))))))
    (setf (type-spec instance)
          (traverse (var-spec instance) (type-spec instance)))))

(defun d-spec-outer-declarations
    (d-spec)
  (let ((result '())
        (variables '()))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                     (unless (eq type t)
                       (push `(type ,type ,var) result))
                     (push var variables))
                   d-spec)
    (setf variables (nreverse variables))
    (when (ignorablep d-spec)
      (push `(ignorable ,@variables) result))
    (when (dynamic-extent-p d-spec)
      (push `(dynamic-extent ,@variables) result))
    (nreverse result)))

(defun d-spec-outer-bindings (d-spec)
  (let ((result '()))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                     (push (list var (deduce-initial-value type))
                           result))
                   d-spec)
    (maphash (lambda (object temp)
               (declare (ignore object))
               (push `(,temp nil) result))
             (temps d-spec))
    (nreverse result)))

(defun d-spec-simple-declarations (d-spec
                                   &key ((:nullable nullablep) nil))
  (with-accessors ((var-spec var-spec)
                   (type-spec type-spec)
                   (ignorablep ignorablep)
                   (dynamic-extent-p dynamic-extent-p))
      d-spec
    (let (decl)
      (when var-spec
        (unless (eq type-spec t)
          (push `(type ,(if nullablep
                             (type-or-null type-spec)
                             type-spec)
                          ,var-spec)
                decl))
        (when ignorablep
          (push `(ignorable ,var-spec) decl))
        (when dynamic-extent-p
          (push `(dynamic-extent ,var-spec) decl)))
      decl)))

(defun d-spec-simple-bindings (d-spec &optional form)
  (with-accessors ((var-spec var-spec)
                   (type-spec type-spec))
      d-spec
    (when var-spec
      `((,var-spec ,form)))))

(defun function-operator-p (value)
  (and (consp value)
       (eq (first value) 'cl:function)
       (cdr value)
       (symbolp (second value))
       (null (cddr value))))

