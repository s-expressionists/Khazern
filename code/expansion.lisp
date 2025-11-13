(cl:in-package #:khazern)

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

(defvar *scopes*)

(defun default-accumulation-variable ()
  (or *accumulation-variable*
      (setf *accumulation-variable* (unique-name :acc))))

(defun get-scope (var)
  (gethash var *scopes*))

(defvar *loop-name*)

(defvar *epilogue-tag*)

(defun prologue-body-epilogue (body-clause)
  (with-unique-names (body)
    (let ((body `(tagbody
                    ,@(prologue-forms body-clause)
                    ,@(step-intro-forms body-clause t)
                    ,@(step-outro-forms body-clause t)
                  ,body
                    ,@(body-forms body-clause)
                    ,@(step-intro-forms body-clause nil)
                    ,@(step-outro-forms body-clause nil)
                    (go ,body)
                  ,*epilogue-tag*
                    ,@(epilogue-forms body-clause)
                    (return-from ,*loop-name*
                      ,*accumulation-variable*)))
          (afterword (afterword-forms body-clause)))
      (if afterword
          `((unwind-protect
                 ,body
              ,@afterword))
          `(,body)))))
    
(defun expand-extended-loop (client)
  (let* ((*accumulation-variable* nil)
         (*index* 0)
         (*tokens* *body*)
         (*toplevel* t)
         (*scopes* (make-hash-table))
         (*extended-superclause* (parse-body client)))
    (analyze client *extended-superclause*)
    (let ((*loop-name* (name *extended-superclause*)))
      `(block ,*loop-name*
         ,@(wrap-forms *extended-superclause*
                       (prologue-body-epilogue *extended-superclause*))))))

(defun expand-simple-loop (client)
  (declare (ignore client))
  (with-unique-names (repeat)
    `(block nil
       (tagbody
        ,repeat
          ,@*body*
          (go ,repeat)))))

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

(defstruct binding-info
  type
  category
  references)

(defun check-variables (client clause)
  (let ((variables (make-hash-table)))
    (map-variables (lambda (name type category references)
                     (multiple-value-bind (info presentp)
                         (gethash name variables)
                       (cond ((not presentp)
                              (setf (gethash name variables)
                                    (make-binding-info :type type :category category
                                                       :references references)))
                             ((and (null category)
                                   (null (binding-info-category info)))
                              (error 'multiple-variable-occurrences
                                     :bound-variable name))
                             ((or (null category)
                                  (null (binding-info-category info)))
                              (error 'iteration-accumulation-overlap
                                     :bound-variable name))
                             ((not (eq category (binding-info-category info)))
                              (error 'multiple-accumulation-occurrences
                                     :bound-variable name
                                     :first-clause category
                                     :second-clause (binding-info-category info)))
                             (t
                              (tagbody
                               next
                                 (when references
                                   (setf (getf (binding-info-references info)
                                               (first references))
                                         (or (getf (binding-info-references info)
                                                   (first references))
                                             (second references))
                                         references (cddr references))
                                   (go next)))
                              (let ((sub12 (subtypep type (binding-info-type info)))
                                    (sub21 (subtypep (binding-info-type info) type)))
                                (unless (and sub12 sub21)
                                  (let ((replacement-type
                                          (cond (sub12 (binding-info-type info))
                                                (sub21 type)
                                                ((and (subtypep type 'number)
                                                      (subtypep (binding-info-type info)
                                                                'number))
                                                 (numeric-super-type type
                                                                     (binding-info-type info)))
                                                (t))))
                                    (warn 'conflicting-types
                                          :name (if (eq name *accumulation-variable*)
                                                    nil
                                                    name)
                                          :type1 type
                                          :type2 (binding-info-type info)
                                          :replacement-type replacement-type)
                                    (setf (binding-info-type info) replacement-type))))))))
                   clause)
    (maphash (lambda (name info)
               (when (binding-info-category info)
                 (let ((instance (make-scope client name (binding-info-type info)
                                                          (binding-info-category info)
                                                          (binding-info-references info))))
                   (setf (gethash name *scopes*) instance)
                   (push instance (subclauses clause)))))
             variables)))

;;; FIXME: Add more analyses.
(defmethod analyze ((client standard-client) (clause extended-superclause))
  (verify-clause-order clause)
  (check-variables client clause))
