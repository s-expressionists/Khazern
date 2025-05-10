(cl:in-package #:khazern)

;;; This variable is bound by the code generator for
;;; CONDITIONAL-CLAUSE before calling the code generators for the
;;; clauses in its THEN and ELSE branches.
(defvar *it-var* nil)

(defvar *accumulation-variable*)

(defvar *tail-variables*)

(defun default-accumulation-variable ()
  (or *accumulation-variable*
      (setf *accumulation-variable* (gensym "ACC"))))

(defun tail-variable (head-variable)
  (let ((result (gethash head-variable *tail-variables*)))
    (when (null result)
      (setf result (gensym "TAIL"))
      (setf (gethash head-variable *tail-variables*) result))
    result))

(defun accumulation-bindings (clauses)
  (let ((bindings nil)
        (variables nil))
    (map-variables (lambda (name type category)
                     (unless (or (eq category t)
                                 (member name variables))
                       (push name variables)
                       (push `(,name
                               ,(case category
                                  (count/sum
                                   (coerce 0 type))
                                  (always/never
                                   t)
                                  (otherwise
                                   nil)))
                             bindings)
                       (when (eq category 'list)
                         (push `(,(tail-variable name) nil)
                               bindings))))
                   clauses)
    (nreverse bindings)))

(defun accumulation-declarations (clauses)
  (let ((declarations nil)
        (variables nil))
    (map-variables (lambda (name type category)
                     (unless (or (eq category t)
                                 (member name variables))
                       (push name variables)
                       (push `(cl:type ,(if (or (not (eq category 'count/sum))
                                                (cl:typep (coerce 0 type) type))
                                            type
                                            `(or (integer 0 0) ,type))
                                       ,name)
                             declarations)))
                   clauses)
    (nreverse declarations)))

(defvar *loop-name*)

(defvar *epilogue-tag*)

(defun prologue-body-epilogue (clauses)
  (let ((start-tag (gensym "BODY")))
    `((tagbody
         ,@(wrap-let* (mapcan #'prologue-bindings clauses)
                      (mapcan #'prologue-declarations clauses)
                      (mapcan #'prologue-forms clauses))
       ,start-tag
         ,@(mapcan #'body-forms clauses)
         ,@(mapcan #'termination-forms clauses)
         ,@(wrap-let* (mapcan #'step-bindings clauses)
                      (mapcan #'step-declarations clauses)
                      (mapcan #'step-forms clauses))
         (go ,start-tag)
       ,*epilogue-tag*
         ,@(mapcan #'epilogue-forms clauses)
         (return-from ,*loop-name*
           ,*accumulation-variable*)))))

;;; Default method for WRAP-SUBCLAUSE.  By default, the wrapper for
;;; each subclause contains only the final bindings, leaving the
;;; initial bindings to a single binding form of the entire clause.
(defmethod wrap-subclause (subclause inner-form)
  (wrap-let (final-bindings subclause)
            (final-declarations subclause)
            inner-form))

;;; Default method for WRAP-CLAUSE.  This method is applicable only if
;;; the clause type does not admit any subclauses.  It
;;; wraps each subclause individually, and then wraps the result in
;;; the initial bindings for the entire clause.
(defmethod wrap-clause (clause inner-form)
  (wrap-let (initial-bindings clause)
            (initial-declarations clause)
            (reduce #'wrap-subclause (subclauses clause)
                    :from-end t :initial-value inner-form)))

;;; Process all clauses by first computing the prologue, the body, and
;;; the epilogue, and then applying the clause-specific wrapper for
;;; each clause to the result.
(defun do-clauses (all-clauses)
  (reduce #'wrap-clause all-clauses
          :from-end t
          :initial-value (prologue-body-epilogue all-clauses)))

(defun expand-clauses (all-clauses)
  (wrap-let (accumulation-bindings all-clauses)
            (accumulation-declarations all-clauses)
            (do-clauses all-clauses)))

(defun expand-body (loop-body epilogue-tag parser-table)
  (cond ((notevery #'listp loop-body)
         (let* ((*accumulation-variable* nil)
                (*epilogue-tag* epilogue-tag)
                (clauses (parse-loop-body loop-body parser-table)))
           (analyze clauses)
           (let* ((name (if (name-clause-p (car clauses))
                            (name (car clauses))
                            nil))
                  (*loop-name* name)
                  (*tail-variables* (make-hash-table :test #'eq)))
             `(block ,name
                ,@(expand-clauses clauses)))))
        ((some #'null loop-body)
         (error 'non-compound-form))
        (t
         (let ((tag (gensym)))
            `(block nil
               (tagbody
                ,tag
                 ,@loop-body
                 (go ,tag)))))))
