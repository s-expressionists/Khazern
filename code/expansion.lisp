(cl:in-package #:khazern)

;;; The purpose of this generic function is to generate a list of all
;;; bound variables in a clause.  The same variable occurs as many
;;; times in the list as the number of times it is bound in the
;;; clause.
(defgeneric bound-variables (clause))

;;; The purpose of this generic function is to generate a list of all
;;; the accumulation variables in a clause.  Each element of the list
;;; is itself a list of three elements.  The first element is the name
;;; of a variable used in an INTO clause, or NIL if the clause has no
;;; INTO.  The second element determines the kind of accumulation, and
;;; can be one of the symbols LIST, COUNT/SUM, or MAX/MIN.  The third
;;; element is a type specifier which can be T.
(defgeneric accumulation-variables (clause))

;;; The purpose of this generic function is to extract a list of
;;; declaration specifiers from the clause.  Notice that it is a list
;;; of declaration specifiers, not a list of declarations.  In other
;;; words, the symbol DECLARE is omitted.
(defgeneric initial-declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-declarations (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric initial-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

(defgeneric final-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    '()))

;;; This generic function returns a binding for CLAUSE that should go in
;;; the LOOP prologue.
(defgeneric prologue-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP prologue.  The INITIALLY clause is an obvious candidate
;;; for such code.  But the stepping clauses also have code that goes
;;; in the prologue, namely an initial termination test to determine
;;; whether any iterations at all should be executed.  END-TAG is the
;;; tag to GO to when the initial termination test says that no
;;; iterations should be executed.
(defgeneric prologue-forms(clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go
;;; between the body code and the stepping forms in the body of the
;;; expanded code.  Some of the FOR-AS clauses and also the REPEAT
;;; clause generate code here.  END-TAG is the tag to GO to when
;;; iteration should terminate.
(defgeneric termination-forms (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the main the body code, before the termination test and the
;;; stepping forms, in the body of the expanded code.  The DO clause
;;; and the accumulation clauses are obvious candidates for such code.
;;;
;;; FIXME: Currently, END-TAG is used only in the WHILE clause as a
;;; termination test.  Investigate whether the WHILE clause should use
;;; TERMINATION-TEST instead, so that we can eliminate this parameter.
(defgeneric body-forms (clause end-tag)
  (:method (clause end-tag)
    (declare (ignore clause end-tag))
    nil))

;;; This generic function returns the bindings for CLAUSE that go
;;; after the main body code and the termination tests in the body of
;;; the expanded code.
(defgeneric step-bindings (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go
;;; after the main body code and the termination tests in the body of
;;; the expanded code.  The FOR-AS clauses and also the REPEAT clause
;;; generate code here.
(defgeneric step-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This generic function returns a form for CLAUSE that should go in
;;; the LOOP epilogue.  Of the clause types defined by the Common Lisp
;;; standard, only the method specialized to the FINALLY clause
;;; returns a value other than NIL.
(defgeneric epilogue-forms (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric main-clause-p (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric name-clause-p (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

(defgeneric variable-clause-p (clause)
  (:method (clause)
    (declare (ignore clause))
    nil))

;;; This variable is bound by the code generator for
;;; CONDITIONAL-CLAUSE before calling the code generators for the
;;; clauses in its THEN and ELSE branches.
(defvar *it-var* nil)

(defvar *accumulation-variable*)

(defvar *list-tail-accumulation-variable*)

(defvar *tail-variables*)

(defun tail-variable (head-variable)
  (let ((result (gethash head-variable *tail-variables*)))
    (when (null result)
      (setf result (gensym))
      (setf (gethash head-variable *tail-variables*) result))
    result))

(defun accumulation-bindings (clauses)
  (let* ((descriptors
           (reduce #'append
                   (mapcar #'accumulation-variables clauses)))
         (equal-fun (lambda (d1 d2)
                      (and (eq (first d1) (first d2))
                           (eq (second d1) (second d2)))))
         (unique (remove-duplicates descriptors :test equal-fun)))
    (loop for (name category type) in unique
          for initial-value = (cond  ((eq category 'count/sum)
                                      (coerce 0 type))
                                     ((eq category 'always/never)
                                      t)
                                     (t
                                      nil))
          collect (if (null name)
                      `(,*accumulation-variable* ,initial-value)
                      `(,name ,initial-value))
          when (eq category 'list)
            collect (if (null name)
                        `(,*list-tail-accumulation-variable* nil)
                        `(,(tail-variable name) nil)))))

(defvar *loop-name*)

(defun prologue-body-epilogue (clauses end-tag)
  (let ((start-tag (gensym)))
    `((tagbody
         ,@(wrap-let* (reduce #'append (mapcar #'prologue-bindings clauses)
                              :from-end t)
                      '()
                      (mapcan (lambda (clause)
                                (prologue-forms clause end-tag))
                              clauses))
       ,start-tag
         ,@(mapcan (lambda (clause)
                            (body-forms clause end-tag))
                          clauses)
         ,@(mapcan (lambda (clause)
                            (termination-forms clause end-tag))
                          clauses)
         ,@(wrap-let* (reduce #'append (mapcar #'step-bindings clauses)
                              :from-end t)
                      '()
                      (mapcan #'step-forms clauses))
         (go ,start-tag)
       ,end-tag
         ,@(mapcan #'epilogue-forms clauses)
         (return-from ,*loop-name*
           ,*accumulation-variable*)))))

;;; Once the LOOP prologue, the LOOP body, and the LOOP epilogue have
;;; all been constructed, a bunch of successive WRAPPERS are applied
;;; so as to obtain the final expansion.  Each clause type defines how
;;; it needs to be wrapped.  Some clauses only require the
;;; establishment of variable bindings in the wrapper.  Other clauses
;;; might need to be wrapped in some iterator form.  The generic
;;; function WRAP-CLAUSE defines how each clause type is wrapped.
(defgeneric wrap-clause (clause inner-form))

;;; If a clause can have subclauses, then each subclause may need to
;;; be wrapped separately.  The generic function WRAP-SUBCLAUSE
;;; determines how this is done.
(defgeneric wrap-subclause (subclause inner-form))

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
(defun do-clauses (all-clauses end-tag)
  (reduce #'wrap-clause all-clauses :from-end t
          :initial-value (prologue-body-epilogue all-clauses end-tag)))

(defun expand-clauses (all-clauses end-tag)
  (let ((acc (accumulation-bindings all-clauses)))
    (wrap-let (if (member *accumulation-variable* acc :key #'car)
                  acc
                  (list* `(,*accumulation-variable* nil)
                         acc))
              nil
              (do-clauses all-clauses end-tag))))

(defun expand-body (loop-body end-tag parser-table)
  (cond ((notevery #'listp loop-body)
         (let ((clauses (parse-loop-body loop-body parser-table)))
           (analyze-clauses clauses)
           (let* ((name (if (name-clause-p (car clauses))
                            (name (car clauses))
                            nil))
                  (*loop-name* name)
                  (*accumulation-variable* (gensym))
                  (*list-tail-accumulation-variable* (gensym))
                  (*tail-variables* (make-hash-table :test #'eq)))
             `(block ,name
                ,@(expand-clauses clauses end-tag)))))
        ((some #'null loop-body)
         (error 'non-compound-form))
        (t
         (let ((tag (gensym)))
            `(block nil
               (tagbody
                ,tag
                 ,@loop-body
                 (go ,tag)))))))
