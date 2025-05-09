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

(defun normalize-keyword (x choices)
  (car (find x choices
             :test (lambda (x y) (find x y :test #'symbol-equal)))))
