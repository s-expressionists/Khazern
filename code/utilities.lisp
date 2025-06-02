(cl:in-package #:khazern)

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

;;; This function is used in the list accumulation clauses COLLECT,
;;; APPEND, and NCONC.  The idea is that CONS cells in a suffix of the
;;; accumulated list must be copied, because they were attached by the
;;; APPEND clause, and so they weren't copied then, but if more CONS
;;; cells must be attached, then they do have to be copied in order
;;; that semantics be preserved.  When LIST-TAIL-ACCUMULATION-VARIABLE
;;; points to a CONS cell (say, C), then this suffix consists of all
;;; the CONS cells in the list pointed to by the CDR of C.  When the
;;; value of LIST-TAIL-ACCUMULATION-VARIABLE is NIL, then the suffix
;;; consists of all the CONS cell accumulated so far, and they make up
;;; the list that is the value of ACCUMULATION-VARIABLE.
(defun copy-cons-cells
    (accumulation-variable list-tail-accumulation-variable)
  `(tagbody
      ;; If the tail variable is NIL, then every CONS cell in the
      ;; list starting at the accumulation variable must be copied,
      ;; and we know that there is at least one.  So we can
      ;; eliminate this special case by copying the first CONS
      ;; cell, and setting the tail variable to point to it.  We
      ;; could call COPY-LIST and then LAST, but then we would
      ;; traverse the list twice, so we do it with a loop instead.
      (when (null ,list-tail-accumulation-variable)
        (setf ,accumulation-variable
              (cons (car ,accumulation-variable)
                    (cdr ,accumulation-variable))
              ,list-tail-accumulation-variable
              ,accumulation-variable))
      ;; At this point, whether the tail variable was initially NIL or
      ;; not, now it no longer is.  And every CONS cell after the one
      ;; that the tail variable points to must be copied.
    again
      (unless (atom (cdr ,list-tail-accumulation-variable))
        ;; We have not copied all of the CONS celll so  we
        ;; copy the CONS cell pointed to by the
        ;; CDR of the tail variable and advance the tail
        ;; variable by one position.
        (setf (cdr ,list-tail-accumulation-variable)
              (cons (cadr ,list-tail-accumulation-variable)
                    (cddr ,list-tail-accumulation-variable))
              ,list-tail-accumulation-variable
              (cdr ,list-tail-accumulation-variable))
        (go again))))

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

(defun numeric-super-type (type)
  (find type *numeric-types* :test #'subtypep))

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

(defclass d-spec ()
  ((%var-spec :accessor var-spec
              :initarg :var-spec)
   (%type-spec :accessor type-spec
               :initarg :type-spec
               :initform t)
   (%accumulation-category :accessor accumulation-category
                           :initarg :accumulation-category
                           :initform t)
   (%ignorable :accessor ignorablep
               :initarg :ignorable
               :initform nil)
   (%temps :accessor temps)))

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
    ((clause d-spec) &rest initargs &key ((:temp-var temp-var-p) nil))
  (declare (ignore initargs))
  (set-d-spec-temps clause temp-var-p))

(defun d-spec-inner-bindings (d-spec form &optional bind-all-p)
  (let ((bindings '())
        (temps (temps d-spec)))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec))
                     ((symbolp d-var-spec)
                      (let ((temp (gethash d-var-spec temps)))
                        (when temp
                          (push `(,temp ,form) bindings))
                        (when bind-all-p
                          (push `(,d-var-spec ,(or temp form)) bindings))))
                     ((consp d-var-spec)
                      (let ((temp (gethash d-var-spec temps)))
                        (cond (temp
                               (push `(,temp ,form) bindings)
                               (traverse (car d-var-spec) `(car ,temp))
                               (traverse (cdr d-var-spec) `(cdr ,temp)))
                              (t
                               (traverse (car d-var-spec) `(car ,form))
                               (traverse (cdr d-var-spec) `(cdr ,form)))))))))
      (traverse (var-spec d-spec) form)
      (nreverse bindings))))

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
      (nreverse assignments))))

(defun d-spec-inner-form (d-spec form)
  (wrap-let* (d-spec-inner-bindings d-spec form)
             nil
             `((setq ,@(d-spec-inner-assignments d-spec form)))))

(defmethod map-variables (function (d-spec d-spec))
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
                    `(or null ,type-spec))))
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
        (ignorables '())
        (ignorablep (ignorablep d-spec)))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                     (unless (eq type t)
                       (push `(type ,type ,var) result))
                     (when ignorablep
                       (push var ignorables)))
                   d-spec)
    (when ignorables
      (push `(ignorable ,.(nreverse ignorables)) result))
    (nreverse result)))

(defun d-spec-outer-bindings (d-spec)
  (let ((result '()))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                     (push (list var (deduce-initial-value type))
                           result))
                   d-spec)
    (nreverse result)))

(defun d-spec-simple-declarations (d-spec
                                   &key ((:ignorable ignorablep) nil)
                                        ((:nullable nullablep) nil))
  (with-accessors ((var-spec var-spec)
                   (type-spec type-spec))
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
          (push `(ignorable ,var-spec) decl)))
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

