(cl:in-package #:khazern)

(defun type-or-null (type)
  (if (cl:typep nil type)
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

(defun var-spec-temps (d-var-spec &optional temp-var-p)
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
      (traverse d-var-spec)
      temps)))

(defun var-spec-bindings (d-var-spec form temps &optional bind-all-p)
  (let ((bindings '()))
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
      (traverse d-var-spec form)
      (nreverse bindings))))

(defun var-spec-assignments (d-var-spec form temps)
  (let ((assignments '()))
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
      (traverse d-var-spec form)
      (nreverse assignments))))

(defun generate-assignments (d-var-spec form)
  (let ((temps (var-spec-temps d-var-spec)))
    (wrap-let* (var-spec-bindings d-var-spec form temps)
               nil
               `((setq ,@(var-spec-assignments d-var-spec form temps))))))

(defun %map-variables (function var-spec type-spec)
  (cond ((null var-spec))
        ((symbolp var-spec)
         (funcall function var-spec (or type-spec t) t))
        ((not (consp var-spec))
         (error 'expected-var-spec-but-found
                :found var-spec))
        ((symbolp type-spec)
         (%map-variables function (car var-spec) type-spec)
         (%map-variables function (cdr var-spec) type-spec))
        ((not (consp type-spec))
         (error 'expected-type-spec-but-found
                :found type-spec))
        (t
         (%map-variables function (car var-spec) (car type-spec))
         (%map-variables function (cdr var-spec) (cdr type-spec))))
  nil)
 
(defun map-variable-types (function var-spec &optional type-spec)
  (let ((result '()))
    (%map-variables (lambda (name type category)
                      (declare (ignore category))
                      (push (funcall function name type) result))
                    var-spec type-spec)
    (nreverse result)))

(defun generate-variable-declarations (var-spec type-spec)
  (map-variable-types (lambda (var type)
                        `(cl:type ,(type-or-null type) ,var))
                      var-spec type-spec))

(defun generate-variable-bindings (var-spec)
  (map-variable-types (lambda (var type)
                        (declare (ignore type))
                        `(,var nil))
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

(defun first-result (&rest rest)
  (first rest))

(defun second-result (&rest rest)
  (second rest))

(defun third-result (&rest rest)
  (third rest))

(defun fourth-result (&rest rest)
  (fourth rest))
