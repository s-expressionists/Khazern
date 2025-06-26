(cl:in-package #:khazern)

(defvar *start*)

(defvar *tokens*)

(defvar *index*)

(deftype simple-type-spec ()
  '(or null (member fixnum float t)))

(defun pop-token (&key (type nil type-p) (keywords nil keywords-p))
  (when (null *tokens*)
    (error 'expected-token-but-end
           :expected-type type
           :expected-keywords keywords
           :location *index*))
  (trivial-with-current-source-form:with-current-source-form
      ((car *tokens*) *tokens*)
    (when (or (and type-p
                   (not (typep (car *tokens*) type)))
              (and keywords-p
                   (or (not (symbolp (car *tokens*)))
                       (not (find (car *tokens*) keywords :test #'symbol-equal)))))
      (error 'expected-token-but-found
             :found (car *tokens*)
             :expected-type type
             :expected-keywords keywords
             :location *index*))
    (incf *index*)
    (pop *tokens*)))

(defun pop-token? (&key (type nil type-p) (keywords nil keywords-p))
  (cond ((and (car *tokens*)
              (or (not type-p)
                  (typep (car *tokens*) type))
              (or (not keywords-p)
                  (find (car *tokens*) keywords :test #'symbol-equal)))
         (incf *index*)
         (values t (pop *tokens*)))
        (t
         (values nil nil))))

(defun push-token (token)
  (decf *index*)
  (push token *tokens*))

(defmethod parse-clause (client scope name)
  (error 'unknown-parser
         :client client
         :scope scope
         :name name))

(defun make-parser-name (client scope token)
  (when (symbolp token)
    (multiple-value-bind (name status)
        (find-symbol (symbol-name token) :keyword)
      (when status
        (return-from make-parser-name name))))
  (error 'unknown-parser
         :client client
         :scope scope
         :name token))

(defun do-parse-clause (client scope &optional (*start* *index*))
  (parse-clause client scope (make-parser-name client scope (pop-token))))

(defun parse-type-spec (&optional (default-type-spec t))
  (if (pop-token? :keywords '(:of-type))
      (pop-token)
      (multiple-value-bind (foundp type-spec)
          (pop-token? :type 'simple-type-spec)
        (if foundp
            type-spec
            default-type-spec))))

(defun parse-d-spec (&key (type-spec t) ((:ignorable ignorablep) nil)
                          ((:dynamic-extent dynamic-extent-p) nil))
  (make-instance 'destructuring-binding
                 :var-spec (pop-token)
                 :type-spec (parse-type-spec type-spec)
                 :ignorable ignorablep
                 :dynamic-extent dynamic-extent-p))

(defun parse-compound-form+ ()
  (prog (forms)
     (push (pop-token :type 'cons) forms)
   next
     (multiple-value-bind (foundp form)
         (pop-token? :type 'cons)
       (when foundp
         (push form forms)
         (go next)))
     (return (nreverse forms))))

(defun parse-parallel-clauses (client scope)
  (prog (clauses)
   next
     (push (do-parse-clause client scope)
           clauses)
     (when (pop-token? :keywords '(:and))
       (go next))
     (return (nreverse clauses))))

(defun parse-body (client)
  (prog ((scope (make-instance 'extended-superclause))
         clauses)
   next
     (push (do-parse-clause client scope) clauses)
     (when *tokens*
       (go next))
     (setf (subclauses scope) (nreverse clauses))
     (return scope)))

(defparameter *placeholder-result* (cons nil nil))

