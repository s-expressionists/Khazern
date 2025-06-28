(cl:in-package #:khazern)

(defvar *start*)

(defvar *tokens*)

(defvar *index*)

(deftype simple-type-spec ()
  '(or null (member fixnum float t)))

(defun find-keyword (token keyword-or-keywords)
  (cond ((listp keyword-or-keywords)
         (some (lambda (keyword)
                 (find-keyword token keyword))
               keyword-or-keywords))
        ((symbol-equal token keyword-or-keywords)
         keyword-or-keywords)))

(defun pop-token (&key (type nil type-p) (keywords nil keywords-p))
  (when (null *tokens*)
    (error 'expected-token-but-end
           :expected-type type
           :expected-keywords keywords
           :location *index*))
  (trivial-with-current-source-form:with-current-source-form
      ((car *tokens*) *tokens*)
    (let ((keyword nil))
      (when (or (and type-p
                     (not (typep (car *tokens*) type)))
                (and keywords-p
                     (or (not (symbolp (car *tokens*)))
                         (not (setf keyword (find-keyword (car *tokens*) keywords))))))
        (error 'expected-token-but-found
               :found (car *tokens*)
               :expected-type type
               :expected-keywords keywords
               :location *index*))
      (incf *index*)
      (values (pop *tokens*) keyword))))

(defun pop-token? (&key (type nil type-p) (keywords nil keywords-p))
  (let ((keyword nil))
    (cond ((or (null *tokens*)
               (and type-p
                    (not (typep (car *tokens*) type)))
               (and keywords-p
                    (or (not (symbolp (car *tokens*)))
                        (not (setf keyword (find-keyword (car *tokens*) keywords))))))
           (values nil nil nil))
          (t
           (incf *index*)
           (values t (pop *tokens*) keyword)))))

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

