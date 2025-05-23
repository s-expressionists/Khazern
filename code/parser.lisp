(cl:in-package #:khazern)

(deftype simple-type-spec ()
  '(or null (member fixnum float t)))

(defclass token-stream ()
  ((%index :accessor index
           :initform 0)
   (%tokens :accessor tokens
            :initarg :tokens)))

(defun pop-token (client scope token-stream
                  &key (type nil type-p) (keywords nil keywords-p))
  (when (null (tokens token-stream))
    (error 'expected-token-but-end
           :location (index token-stream)))
  (trivial-with-current-source-form:with-current-source-form
      ((car (tokens token-stream)) (tokens token-stream))
    (when (and type-p
               (not (typep (car (tokens token-stream)) type)))
      (error 'expected-token-but-found
             :found (car (tokens token-stream))
             :expected-type type
             :location (index token-stream)))
    (when (and keywords-p
               (or (not (symbolp (car (tokens token-stream))))
                   (not (find (car (tokens token-stream)) keywords :test #'symbol-equal))))
      (error 'expected-token-but-found
             :found (car (tokens token-stream))
             :expected-keywords keywords
             :location (index token-stream)))
    (incf (index token-stream))
    (pop (tokens token-stream))))

(defun pop-token? (client scope token-stream
                   &key (type nil type-p) (keywords nil keywords-p))
  (cond ((and (car (tokens token-stream))
              (or (not type-p)
                  (typep (car (tokens token-stream)) type))
              (or (not keywords-p)
                  (find (car (tokens token-stream)) keywords :test #'symbol-equal)))
         (incf (index token-stream))
         (values t (pop (tokens token-stream))))
        (t
         (values nil nil))))

(defmethod parse-tokens (client scope name tokens)
  (declare (ignore tokens))
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

(defun do-parse-tokens (client scope tokens)
  (parse-tokens client scope (make-parser-name client scope (pop-token client scope tokens)) tokens))

(defun parse-type-spec (client scope tokens &optional (default-type-spec t))
  (if (pop-token? client scope tokens :keywords '(:of-type))
      (pop-token client scope tokens)
      (multiple-value-bind (foundp type-spec)
          (pop-token? client scope tokens :type 'simple-type-spec)
        (if foundp
            type-spec
            default-type-spec))))

(defun parse-d-spec (client scope tokens &key (type-spec t) (accumulation-category t))
  (make-instance 'd-spec
                 :var-spec (pop-token client scope tokens)
                 :type-spec (parse-type-spec client scope tokens type-spec)
                 :accumulation-category accumulation-category))

(defun parse-compound-form+ (client scope tokens)
  (prog (forms)
     (push (pop-token client scope tokens :type 'cons) forms)
   next
     (multiple-value-bind (foundp form)
         (pop-token? client scope tokens :type 'cons)
       (when foundp
         (push form forms)
         (go next)))
     (return (nreverse forms))))

(defun parse-parallel-clauses (client scope tokens)
  (prog (clauses)
   next
     (push (do-parse-tokens client scope tokens)
           clauses)
     (when (pop-token? client scope tokens :keywords '(:and))
       (go next))
     (return (nreverse clauses))))

(defun parse-body (client tokens)
  (prog ((scope (make-instance 'body-clauses))
         clauses)
   next
     (push (do-parse-tokens client scope tokens) clauses)
     (when (tokens tokens)
       (go next))
     (return (nreverse clauses))))

(defparameter *placeholder-result* (cons nil nil))

