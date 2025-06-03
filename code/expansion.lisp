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
                       (push `(type ,(if (or (not (eq category 'count/sum))
                                                (typep (coerce 0 type) type))
                                            type
                                            `(or (integer 0 0) ,type))
                                       ,name)
                             declarations)))
                   clauses)
    (nreverse declarations)))

(defvar *loop-name*)

(defvar *epilogue-tag*)

(defun prologue-body-epilogue (body-clause)
  (let ((start-tag (gensym "BODY")))
    `((tagbody
         ,@(prologue-forms body-clause)
         ,@(initial-step-forms body-clause)
       ,start-tag
         ,@(body-forms body-clause)
         ,@(subsequent-step-forms body-clause)
         (go ,start-tag)
       ,*epilogue-tag*
         ,@(epilogue-forms body-clause)
         (return-from ,*loop-name*
           ,*accumulation-variable*)))))

(defun expand-extended-loop (client loop-body)
  (let* ((*accumulation-variable* nil)
         (body-clause (parse-body client
                                  (make-instance 'token-stream
                                                 :tokens loop-body))))
    (analyze body-clause)
    (let ((*loop-name* (if (name-clause-p (car (subclauses body-clause)))
                           (name (car (subclauses body-clause)))
                           nil))
          (*tail-variables* (make-hash-table :test #'eq)))
      `(block ,*loop-name*
         ,@(wrap-let (accumulation-bindings body-clause)
                     (accumulation-declarations body-clause)
                     (wrap-forms body-clause
                                 (prologue-body-epilogue body-clause)))))))

(defun expand-simple-loop (client loop-body)
  (declare (ignore client))
  (let ((tag (gensym)))
    `(block nil
       (tagbody
        ,tag
          ,@loop-body
          (go ,tag)))))

(defun expand-body (client loop-body *epilogue-tag*)
  (trivial-with-current-source-form:with-current-source-form (loop-body)
    (cond ((notevery #'listp loop-body)
           (expand-extended-loop client loop-body))
          ((some #'null loop-body)
           (error 'non-compound-form))
          (t
           (expand-simple-loop client loop-body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

;;; Check that if there is a name-clause, the last one is in position
;;; zero.
(defun check-name-clause-position (clauses)
  (let ((name-clause-position
          (position-if #'name-clause-p clauses :from-end t)))
    (when (and (not (null name-clause-position)) (plusp name-clause-position))
      (error 'name-clause-not-first))))

;;; Check that there is not a variable-clause following a main clause.
;;; Recall that we diverge from the BNF grammar in the HyperSpec so
;;; that INITIALLY and FINALLY are neither main clauses nor variable
;;; clauses.
(defun check-order-variable-clause-main-clause (clauses)
  (let ((last-variable-clause-position
          (position-if #'variable-clause-p clauses :from-end t))
        (first-main-clause-position
          (position-if #'main-clause-p clauses)))
    (when (and (not (null last-variable-clause-position))
               (not (null first-main-clause-position))
               (> last-variable-clause-position first-main-clause-position))
      (error 'invalid-clause-order))))

(defun verify-clause-order (clauses)
  (check-name-clause-position clauses)
  (check-order-variable-clause-main-clause clauses))

(defun check-variables (clauses)
  (let ((variables nil))
    (map-variables (lambda (name type category)
                     (declare (ignore type))
                     (let ((current-category (getf variables name)))
                       (cond ((null current-category)
                              (setf (getf variables name) category))
                             ((and (eq category t)
                                   (eq current-category t))
                              (error 'multiple-variable-occurrences
                                     :bound-variable name))
                             ((or (eq category t)
                                  (eq current-category t))
                              (error 'iteration-accumulation-overlap
                                     :bound-variable name))
                             ((not (eq category current-category))
                              (error 'multiple-accumulation-occurrences
                                     :bound-variable name
                                     :first-clause category
                                     :second-clause current-category)))))
                   clauses)))

;;; FIXME: Add more analyses.
(defmethod analyze ((clause extended-superclause))
  (verify-clause-order (subclauses clause))
  (check-variables clause))
