(cl:in-package #:khazern)

(defvar *start*)

(defvar *tokens*)

(defvar *index*)

(defvar *toplevel*)

(defvar *body*)

(defun find-keyword (token keyword-or-keywords)
  (cond ((listp keyword-or-keywords)
         (some (lambda (keyword)
                 (find-keyword token keyword))
               keyword-or-keywords))
        ((symbol-equal token keyword-or-keywords)
         keyword-or-keywords)))

(defun flatten-keywords (keyword-or-keywords)
  (if (listp keyword-or-keywords)
      (mapcan #'flatten-keywords keyword-or-keywords)
      (list keyword-or-keywords)))

(defun parse-token (&key (type nil type-p) (keywords nil keywords-p))
  (when (null *tokens*)
    (error 'expected-token-but-end
           :expected-type type
           :expected-keywords (flatten-keywords keywords)
           :clause (when (numberp *start*)
                     (subseq *body* *start* *index*))))
  (trivial-with-current-source-form:with-current-source-form ((car *tokens*))
    (let ((keyword nil))
      (when (or (and type-p
                     (not (typep (car *tokens*) type)))
                (and keywords-p
                     (or (not (symbolp (car *tokens*)))
                         (not (setf keyword (find-keyword (car *tokens*) keywords))))))
        (error 'expected-token-but-found
               :found (car *tokens*)
               :expected-type type
               :expected-keywords (flatten-keywords keywords)
               :clause (when (numberp *start*)
                         (subseq *body* *start* *index*))))
      (when *toplevel*
        (incf *index*))
      (values (pop *tokens*) keyword))))

(defun maybe-parse-token (&key (type nil type-p) (keywords nil keywords-p))
  (let ((keyword nil))
    (cond ((or (null *tokens*)
               (and type-p
                    (not (typep (car *tokens*) type)))
               (and keywords-p
                    (or (not (symbolp (car *tokens*)))
                        (not (setf keyword (find-keyword (car *tokens*) keywords))))))
           (values nil nil nil))
          (t
           (when *toplevel*
             (incf *index*))
           (values t (pop *tokens*) keyword)))))

(defun unparse-token (token)
  (when *toplevel*
    (decf *index*))
  (push token *tokens*))

(defun parse-clause-error (client region found)
  (error 'expected-token-but-found
         :found found
         :expected-keywords (let (keywords)
                              (mapc (lambda (method)
                                      (destructuring-bind (client-specializer region-specializer
                                                           name-specializer)
                                          (closer-mop:method-specializers method)
                                        (when (and (typep client client-specializer)
                                                   (typep region region-specializer)
                                                   (typep name-specializer 'closer-mop:eql-specializer))
                                          (pushnew (closer-mop:eql-specializer-object name-specializer)
                                                   keywords))))
                                    (closer-mop:generic-function-methods #'parse-clause))
                              (sort keywords #'string< :key #'string))))

(defmethod parse-clause (client region name &key &allow-other-keys)
  (parse-clause-error client region name))

(defun make-parser-name (client region token)
  (when (symbolp token)
    (multiple-value-bind (name status)
        (find-symbol (symbol-name token) :keyword)
      (when status
        (return-from make-parser-name name))))
  (parse-clause-error client region token))

(defun do-parse-clause (client region *start* read-name-p &rest args)
  (apply #'parse-clause
         client region
         (when read-name-p
           (make-parser-name client region (parse-token)))
         args))

(defmethod parse-type-spec
    ((client standard-client) var-spec simplep &key)
  (declare (ignore simplep))
  (multiple-value-bind (foundp type-spec)
      (maybe-parse-token :type 'simple-type-spec)
    (when foundp
      (setf (type-spec var-spec) type-spec))))

(defmethod parse-type-spec
    ((client standard-client) var-spec (simplep null) &key)
  (setf (type-spec var-spec) (parse-token :type 'd-type-spec)))

(defmethod parse-var-spec ((client standard-client) &key (type-spec t))
  (make-instance 'destructuring-binding
                 :var-spec (parse-token :type 'd-var-spec)
                 :type-spec type-spec
                 :ignorable t))

(defun parse-t-spec (client binding &rest args)
  (apply #'parse-type-spec
         client binding
         (not (maybe-parse-token :keywords '(:of-type)))
         args))

(defun parse-d-spec (client &rest args)
  (let ((binding (apply #'parse-var-spec client args)))
    (parse-t-spec client binding)
    binding))

(defun parse-compound-forms ()
  (prog (forms)
     (push (parse-token :type 'cons) forms)
   next
     (multiple-value-bind (foundp form)
         (maybe-parse-token :type 'cons)
       (when foundp
         (push form forms)
         (go next)))
     (return (nreverse forms))))

(defmacro parse-conjunctive-clauses (client region read-name-p &rest args)
  (with-unique-names (clauses next)
    `(prog (,clauses)
      ,next
        (push (do-parse-clause ,client ,region *index* ,read-name-p ,@args)
              ,clauses)
        (when (maybe-parse-token :keywords '(:and))
          (go ,next))
        (return (nreverse ,clauses)))))

(defun parse-body (client)
  (prog ((region (make-instance 'extended-superclause))
         clauses)
   next
     (push (do-parse-clause client region *index* t) clauses)
     (when *tokens*
       (go next))
     (setf (subclauses region) (nreverse clauses))
     (return region)))

(defparameter *placeholder-result* (cons nil nil))

(defun parse-into (client
                   &key (default-type-spec t) ((:parse-type-spec parse-type-spec-p) nil)
                        category scope-references)
  (let ((binding (make-instance 'simple-binding
                                :var-spec (if (maybe-parse-token :keywords '(:into))
                                              (parse-token :type 'symbol)
                                              (default-accumulation-variable))
                                :type-spec default-type-spec
                                :category category
                                :scope-references scope-references)))
    (when parse-type-spec-p
      (parse-t-spec client binding))
    binding))

(defmacro with-tokens (tokens &body body)
  `(let ((*tokens* ,tokens)
         (*toplevel* nil))
     (trivial-with-current-source-form:with-current-source-form (*tokens*)
       ,@body)))

(defun more-tokens-p ()
  (and *tokens* t))

(defun parse-usings (client instance using-names using)
  (trivial-with-current-source-form:with-current-source-form (using)
    (prog ((*tokens* using)
           (*toplevel* nil)
           keyword)
     next-using
       (unless using-names
         (error 'unexpected-token-found
                :found (car *tokens*)
                :clause (subseq *body* *start* *index*)))
       (setf keyword (nth-value 1 (parse-token :keywords using-names))
             using-names (delete-name keyword using-names))
       (parse-using client instance keyword)
     (when *tokens*
       (go next-using)))))

(defun parse-prepositions (client instance)
  (multiple-value-bind (preposition-names required-preposition-names using-names)
      (preposition-names client instance)
    (setf preposition-names (copy-list preposition-names)
          required-preposition-names (copy-list required-preposition-names)
          using-names (copy-list using-names))
    (when using-names
      (pushnew :using preposition-names))
    (prog ((foundp nil)
           (token nil)
           (keyword nil))
     next-preposition
       (when preposition-names
         (cond (required-preposition-names
                (multiple-value-setq (token keyword)
                  (parse-token :keywords preposition-names))
                (setf foundp t))
               (t
                (multiple-value-setq (foundp token keyword)
                  (maybe-parse-token :keywords preposition-names))))
         (when foundp
           (setf preposition-names (delete-name keyword preposition-names)
                 required-preposition-names (delete-name keyword required-preposition-names))
           (if (eq keyword :using)
               (parse-usings client instance using-names (parse-token :type 'cons))
               (parse-preposition client instance keyword))
           (go next-preposition)))))
  (setf (end instance) *index*)
  instance)

(defun parse-accumulation (client instance
                           &key (default-type-spec t) ((:parse-type-spec parse-type-spec-p) nil)
                                category scope-references)
  (setf (start instance) *start*
        (form instance) (parse-token)
        (var instance) (parse-into client
                                   :default-type-spec default-type-spec
                                   :parse-type-spec parse-type-spec-p
                                   :category category
                                   :scope-references scope-references))
  (parse-prepositions client instance)
  instance)

