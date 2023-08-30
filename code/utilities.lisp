(cl:in-package #:khazern)

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

;;; This function generates code for destructuring a value according
;;; to a tree of variables.  D-VAR-SPEC is a tree of variable names
;;; (symbols).  FORM is a form that, at runtime, computes the value to
;;; be assigned to the root of D-VAR-SPEC.  This function returns a
;;; list of bindings to be used in a LET* form.  These bindings
;;; destructure the root value until the leaves of the tree are
;;; reached, generating intermediate temporary variables as necessary.
(defun destructure-variables (d-var-spec form)
  (let ((bindings '()))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec)
                      nil)
                     ((symbolp d-var-spec)
                      (push `(,d-var-spec ,form) bindings))
                     ((not (consp d-var-spec))
                      (error 'zexpected-var-spec-but-found
                             :found d-var-spec))
                     (t
                      (let ((temp (gensym)))
                        (push `(,temp ,form) bindings)
                        (traverse (car d-var-spec) `(car ,temp))
                        (traverse (cdr d-var-spec) `(cdr ,temp)))))))
      (traverse d-var-spec form)
      (reverse bindings))))

;;; Given a D-VAR-SPEC, compute a D-VAR-SPEC with the same structure
;;; as the one given as argument, except that the non-NIL leaves
;;; (i.e., the variables names) have been replaced by fresh symbols.
;;; Return two values: the new D-VAR-SPEC and a dictionary in the form
;;; of an association list that gives the correspondence between the
;;; original and the new variables.
(defun fresh-variables (d-var-spec)
  (let* ((dictionary '()))
    (labels ((traverse (d-var-spec)
               (cond ((null d-var-spec)
                      nil)
                     ((symbolp d-var-spec)
                      (let ((temp (gensym)))
                        (push (cons d-var-spec temp) dictionary)
                        temp))
                     (t
                      (cons (traverse (car d-var-spec))
                            (traverse (cdr d-var-spec)))))))
      (values (traverse d-var-spec)
              (reverse dictionary)))))

(defun generate-assignments (d-var-spec form)
  (multiple-value-bind (temp-d-var-spec dictionary)
      (fresh-variables d-var-spec)
    (when dictionary
      `((let* ,(destructure-variables temp-d-var-spec form)
          (setq ,@(loop for (orig-var . temp-var) in dictionary
                        append `(,orig-var ,temp-var))))))))

(defun map-variable-types (function var-spec &optional type-spec)
  (let ((result '()))
    (labels ((extract-aux (var-spec type-spec)
               (cond ((null var-spec)
                      nil)
                     ((symbolp var-spec)
                      (push (funcall function var-spec (or type-spec t)) result))
                     ((symbolp type-spec)
                      (unless (consp var-spec)
                        (error 'expected-var-spec-but-found
                               :found var-spec))
                      (extract-aux (car var-spec) type-spec)
                      (extract-aux (cdr var-spec) type-spec))
                     ((not (consp var-spec))
                      (error 'expected-var-spec-but-found
                             :found var-spec))
                     ((not (consp type-spec))
                      (error 'expected-type-spec-but-found
                             :found type-spec))
                     (t
                      (extract-aux (car var-spec) (car type-spec))
                      (extract-aux (cdr var-spec) (cdr type-spec))))))
      (extract-aux var-spec type-spec)
      result)))

(defun generate-variable-declarations (var-spec type-spec)
  (map-variable-types (lambda (var type)
                        `(cl:type (or null ,type) ,var))
                      var-spec type-spec))

(defun generate-variable-bindings (var-spec)
  (map-variable-types (lambda (var type)
                        (declare (ignore type))
                        `(,var nil))
                      var-spec))

(defun extract-variables (var-spec)
  (map-variable-types (lambda (var type)
                        (declare (ignore type))
                        var)
                      var-spec))

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
                    (cdr ,accumulation-variable)))
        (setf ,list-tail-accumulation-variable
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
                    (cddr ,list-tail-accumulation-variable)))
        (setf ,list-tail-accumulation-variable
              (cdr ,list-tail-accumulation-variable))
        (go again))))

(defun first-result (&rest rest)
  (first rest))

(defun second-result (&rest rest)
  (second rest))

(defun third-result (&rest rest)
  (third rest))

(defun fourth-result (&rest rest)
  (fourth rest))

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
