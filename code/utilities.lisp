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

(defun wrap-let (variable-list declarations forms)
  (cond ((and variable-list declarations)
         `((let ,variable-list
             (declare ,@declarations)
             ,@forms)))
        (variable-list
         `((let ,variable-list
             ,@forms)))
        (declarations
         `((locally
               (declare ,@declarations)
             ,@forms)))
        (t
         forms)))

(defun wrap-let* (variable-list declarations forms)
  (cond ((and variable-list declarations)
         `((let* ,variable-list
             (declare ,@declarations)
             ,@forms)))
        (variable-list
         `((let* ,variable-list
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

(deftype d-var-spec ()
  `(satisfies d-var-spec-p))

(defvar *var-spec* nil)

(defun d-type-spec-p (object &optional (var-spec *var-spec*))
  (or (symbolp var-spec)
      (symbolp object)
      (and (consp var-spec)
           (consp object)
           (d-type-spec-p (car object) (car var-spec))
           (d-type-spec-p (cdr object) (cdr var-spec)))))

(deftype d-type-spec ()
  `(satisfies d-type-spec-p))

(defun function-operator-p (value)
  (and (consp value)
       (eq (first value) 'cl:function)
       (cdr value)
       (symbolp (second value))
       (null (cddr value))))

