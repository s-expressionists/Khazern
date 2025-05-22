(cl:in-package #:khazern)

(defclass token-stream ()
  ((%index :accessor index
           :initform 0)
   (%tokens :accessor tokens
            :initarg :tokens)))

(defun pop-token (client scope token-stream
                  &optional (expected-type nil expected-type-p))
  (when (null (tokens token-stream))
    (error "No more tokens"))
  (when (and expected-type-p
             (not (cl:typep (normalize-token client scope (car (tokens token-stream)))
                         expected-type)))
    (error 'type-error :datum (car (tokens token-stream)) :expected-type expected-type))
  (incf (index token-stream))
  (pop (tokens token-stream)))

(defun pop-token? (client scope token-stream 
                  &optional (expected-type nil expected-type-p))
  (cond ((or (null (tokens token-stream))
             (and expected-type-p
                  (not (cl:typep (normalize-token client scope (car (tokens token-stream)))
                              expected-type))))
         (values nil nil))
        (t
         (incf (index token-stream))
         (values t (pop (tokens token-stream))))))

(defun do-parse-tokens (client scope tokens)
  (parse-tokens client scope (normalize-token client scope (pop-token client scope tokens)) tokens))

(defun parse-type-spec (client scope tokens &optional (default-type-spec t))
  (if (pop-token? client scope tokens '(eql :of-type))
      (pop-token client scope tokens)
      (multiple-value-bind (foundp type-spec)
          (pop-token? client scope tokens '(member fixnum float t null))
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
     (push (pop-token client scope tokens 'cons) forms)
   next
     (multiple-value-bind (foundp form)
         (pop-token? client scope tokens 'cons)
       (when foundp
         (push form forms)
         (go next)))
     (return (nreverse forms))))

(defun parse-parallel-clauses (client scope tokens)
  (prog (clauses)
   next
     (push (do-parse-tokens client scope tokens)
           clauses)
     (when (pop-token? client scope tokens '(eql :and))
       (go next))
     (return (nreverse clauses))))

(defun do-parse-body (client tokens)
  (prog ((scope (make-instance 'body-clauses))
         clauses)
   next
     (push (do-parse-tokens client scope tokens) clauses)
     (when (tokens tokens)
       (go next))
     (return (nreverse clauses))))

(defparameter *empty-result* (cons nil nil))

(defparameter *placeholder-result* (cons nil nil))

