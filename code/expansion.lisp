(cl:in-package #:khazern)

(defvar *body*)

;;; This variable is bound by the code generator for
;;; CONDITIONAL-CLAUSE before calling the code generators for the
;;; clauses in its THEN and ELSE branches.
(defvar *it-var* nil)

(defun it-form (form)
  (if (and *it-var* (it-keyword-p form))
      *it-var*
      form))

(defvar *accumulation-variable*)

(defvar *extended-superclause*)

(defun default-accumulation-variable ()
  (or *accumulation-variable*
      (setf *accumulation-variable* (gensym "ACC"))))

(defun accumulation-reference (var ref)
  (accumulation-scope-reference *extended-superclause* var ref))

(defvar *loop-name*)

(defvar *epilogue-tag*)

(defun prologue-body-epilogue (body-clause)
  (let* ((start-tag (gensym "BODY"))
         (body `(tagbody
                   ,@(prologue-forms body-clause)
                   ,@(step-intro-forms body-clause t)
                   ,@(step-outro-forms body-clause t)
                 ,start-tag
                   ,@(body-forms body-clause)
                   ,@(step-intro-forms body-clause nil)
                   ,@(step-outro-forms body-clause nil)
                   (go ,start-tag)
                 ,*epilogue-tag*
                   ,@(epilogue-forms body-clause)
                   (return-from ,*loop-name*
                     ,*accumulation-variable*)))
         (afterword (afterword-forms body-clause)))
    (if afterword
        `((unwind-protect
               ,body
            ,@afterword))
        `(,body))))
    
(defun expand-extended-loop (client)
  (let* ((*accumulation-variable* nil)
         (*index* 0)
         (*tokens* *body*)
         (*toplevel* t)
         (*extended-superclause* (parse-body client)))
    (analyze *extended-superclause*)
    (let ((*loop-name* (name *extended-superclause*)))
      `(block ,*loop-name*
         ,@(wrap-forms *extended-superclause*
                       (prologue-body-epilogue *extended-superclause*))))))

(defun expand-simple-loop (client)
  (declare (ignore client))
  (let ((tag (gensym)))
    `(block nil
       (tagbody
        ,tag
          ,@*body*
          (go ,tag)))))

(defun expand-body (client *body* *epilogue-tag*)
  (trivial-with-current-source-form:with-current-source-form (*body*)
    (cond ((notevery #'listp *body*)
           (expand-extended-loop client))
          ((some #'null *body*)
           (error 'non-compound-form))
          (t
           (expand-simple-loop client)))))

;;; Syntactic and semantic analysis

(defun verify-clause-order (clause)
  (let ((clauses (subclauses clause)))
    (let ((name-clauses (remove-if (lambda (clause)
                                     (not (eq (clause-group clause) :name)))
                                   clauses)))
      (when (cdr name-clauses)
        (error 'multiple-name-clauses
               :clauses (mapcar (lambda (clause)
                                  (subseq *body* (start clause) (end clause)))
                                name-clauses))))
    (when (eq (clause-group (first clauses)) :name)
      (pop clauses))
    (reduce (lambda (clause group)
              (if (eq (clause-group clause) :variable)
                  :variable
                  (setf (clause-group clause) group)))
            clauses
            :initial-value :main :from-end t)))

(defun check-variables (clause)
  (let ((variables (make-hash-table)))
    (map-variables (lambda (name type category)
                     (multiple-value-bind (pair presentp)
                         (gethash name variables)
                       (cond ((not presentp)
                              (setf (gethash name variables) (cons type category)))
                             ((and (null category)
                                   (null (cdr pair)))
                              (error 'multiple-variable-occurrences
                                     :bound-variable name))
                             ((or (null category)
                                  (null (cdr pair)))
                              (error 'iteration-accumulation-overlap
                                     :bound-variable name))
                             ((not (eq category (cdr pair)))
                              (error 'multiple-accumulation-occurrences
                                     :bound-variable name
                                     :first-clause category
                                     :second-clause (cdr pair)))
                             (t
                              (let ((sub12 (subtypep type (car pair)))
                                    (sub21 (subtypep (car pair) type)))
                                (unless (and sub12 sub21)
                                  (let ((replacement-type
                                          (cond (sub12 (car pair))
                                                (sub21 type)
                                                ((and (subtypep type 'number)
                                                      (subtypep (car pair) 'number))
                                                 (numeric-super-type type (car pair)))
                                                (t))))
                                    (warn 'conflicting-types
                                          :name (if (eq name *accumulation-variable*)
                                                    nil
                                                    name)
                                          :type1 type
                                          :type2 (car pair)
                                          :replacement-type replacement-type)
                                    (setf (car pair) replacement-type))))))))
                   clause)
    (maphash (lambda (name pair)
               (when (cdr pair)
                 (push (make-accumulation-scope name (car pair) (cdr pair))
                       (subclauses clause))))
             variables)))

;;; FIXME: Add more analyses.
(defmethod analyze ((clause extended-superclause))
  (verify-clause-order clause)
  (check-variables clause))
