(cl:in-package #:khazern)

;;; 6.1.2.1 FOR-AS-CLAUSE clause
;;;
;;; The HyperSpec says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;    for-as-clause ::= {for | as} for-as-subclause {and for-as-subclause}* 
;;;    for-as-subclause::= for-as-arithmetic | for-as-in-list | 
;;;                        for-as-on-list | for-as-equals-then | 
;;;                        for-as-across | for-as-hash | for-as-package 
;;;
;;; For the purpose of specialization, we need different names for the
;;; main clauses as well as for the subclauses, so we alter this
;;; grammar a bit and define it like this instead:
;;;
;;;    for-as-clause::= 
;;;      for-as-arithmetic-clause | for-as-in-list-clause | 
;;;      for-as-on-list-clause | for-as-equals-then-clause | 
;;;      for-as-across-clause | for-as-hash-clause | for-as-package-clause
;;;    
;;;    for-as-arithmetic-clause ::=
;;;      {for | as} for-as-arithmetic {and for-as-subclause}* 
;;;    
;;;    for-as-in-list-clause ::=
;;;      {for | as} for-as-in-list {and for-as-subclause}* 
;;;    
;;;    for-as-on-list-clause ::=
;;;      {for | as} for-as-on-list {and for-as-subclause}* 
;;;    
;;;    for-as-equals-then-clause ::=
;;;      {for | as} for-as-equals-then {and for-as-subclause}* 
;;;    
;;;    for-as-across-clause ::=
;;;      {for | as} for-as-across {and for-as-subclause}* 
;;;
;;;    for-as-hash-clause ::=
;;;      {for | as} for-as-hash {and for-as-subclause}* 
;;;
;;;    for-as-package-clause ::=
;;;      {for | as} for-as-package {and for-as-subclause}* 

(defclass for-as-clause (variable-clause subclauses-mixin)
  ())

(defclass for-as-subclause (var-mixin)
  ())

(defmethod analyze ((clause for-as-subclause))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t)))

;;; FOR-AS-CLAUSE parsers

(defmethod parse-tokens ((client standard-client) (scope body-clauses) (keyword (eql :for)) tokens)
  (prog ((instance (make-instance 'for-as-clause))
         subclauses
         var)
   next
     (setf var (parse-d-spec client scope tokens
                             :type-spec *placeholder-result*))
     (push (do-parse-tokens client instance tokens)
           subclauses)
     (setf (var (car subclauses)) var)
     (when (pop-token? client instance tokens :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses))
     (return instance)))

(defmethod parse-tokens ((client standard-client) (scope body-clauses) (keyword (eql :as)) tokens)
  (prog ((instance (make-instance 'for-as-clause))
         subclauses
         var)
   next
     (setf var (parse-d-spec client scope tokens
                             :type-spec *placeholder-result*))
     (push (do-parse-tokens client instance tokens)
           subclauses)
     (setf (var (car subclauses)) var)
     (when (pop-token? client instance tokens :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses))
     (return instance)))

;; FOR-AS-CLAUSE expansion methods

(defmethod map-variables (function (clause for-as-clause))
  (map-variables function (subclauses clause)))

;;; 6.1.2.1.1 FOR-AS-ARITHMETIC sublause

(defclass for-as-arithmetic (for-as-subclause)
  (;; The order in which the forms are given.  This is a list of three
   ;; elements FROM, TO, and BY in the order that they were given in
   ;; the clause.
   (%order :accessor order
           :initarg :order
           :initform nil)
   ;; The form that was given after one of the LOOP keywords FROM,
   ;; UPFROM, or DOWNFROM, or 0 if none of these LOOP keywords was
   ;; given.
   (%start-form :accessor start-form
                :initarg :start-form
                :initform 0)
   ;; The form that was after one of the LOOP keywords TO, UPTO,
   ;; DOWNTO, BELOW, or ABOVE, or NIL if none of these LOOP keywords
   ;; was given.
   (%end-form :accessor end-form
              :initarg :end-form
              :initform nil)
   ;; The form that was after the LOOP keyword BY, or 0 if this
   ;; keyword was not given.
   (%by-form :accessor by-form
             :initarg :by-form
             :initform 1)
   ;; This variable is one step ahead of the iteration variable, and
   ;; when the iteration variable is NIL, the value of this variable
   ;; is never assigned to any iteration variable.
   (%next-var :reader next-var
              :initform (make-instance 'd-spec
                                       :var-spec (gensym "NEXT")
                                       :type-spec 'number))
   (%end-var :reader end-var
             :initform (make-instance 'd-spec
                                      :var-spec (gensym "END")
                                      :type-spec 'number))
   (%by-var :reader by-var
            :initform (make-instance 'd-spec
                                     :var-spec (gensym "BY")
                                     :type-spec 'number))
   ;; If termination is TO, UPTO, or DOWNTO, then this slot contains
   ;; the symbol <=.  If termination is ABOVE or BELOW, then this slot
   ;; contains the symbol <.  If there is TO/UPTO/DOWNTO/ABOVE/BELOW,
   ;; then the loop does not terminate because of this clause, and
   ;; then this slot contains NIL.
   (%termination-test :accessor termination-test
                      :initarg :termination-test
                      :initform nil)))

(defclass for-as-arithmetic-up (for-as-arithmetic)
  ())

(defclass for-as-arithmetic-down (for-as-arithmetic)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FOR-AS-ARITHMETIC parsers

(defun parse-for-as-arithmetic (client scope tokens keyword keywords1 keywords2)
  (let ((instance (make-instance 'for-as-arithmetic)))
    (labels ((handle-preposition (keyword)
               ;; parse the form
               (ecase keyword
                 ((:to :upto :downto :above :below)
                  (setf (end-form instance) (pop-token client scope tokens))
                  (push :to (order instance)))
                 ((:from :downfrom :upfrom)
                  (setf (start-form instance) (pop-token client scope tokens))
                  (push :from (order instance)))
                 (:by
                  (setf (by-form instance) (pop-token client scope tokens))
                  (push :by (order instance))))
               ;; set the termination test
               (case keyword
                 ((:to :upto :downto)
                  (setf (termination-test instance) '<=))
                 ((:above :below)
                  (setf (termination-test instance) '<)))
               ;; set the direction
               (case keyword
                 ((:upto :upfrom :below)
                  (cond ((typep instance 'for-as-arithmetic-down)
                         (error "Incompatible directions"))
                        ((not (typep instance 'for-as-arithmetic-up))
                         (change-class instance 'for-as-arithmetic-up))))
                 ((:downto :downfrom :above)
                  (cond ((typep instance 'for-as-arithmetic-up)
                         (error "Incompatible directions"))
                        ((not (typep instance 'for-as-arithmetic-down))
                         (change-class instance 'for-as-arithmetic-down))))))
             (parse-preposition (keywords)
               (multiple-value-bind (foundp token)
                   (pop-token? client scope tokens :keywords keywords)
                 (when foundp
                   (handle-preposition (intern (symbol-name token) :keyword)))
                 foundp)))
      (handle-preposition keyword)
      (let ((keywords1-p (parse-preposition keywords1)))
        (parse-preposition keywords2)
        (unless keywords1-p
          (parse-preposition keywords1))))
    (pushnew :by (order instance))
    (pushnew :to (order instance))
    (pushnew :from (order instance))
    (setf (order instance) (nreverse (order instance)))
    (unless (typep instance '(or for-as-arithmetic-down for-as-arithmetic-up))
      (change-class instance 'for-as-arithmetic-up))
    instance))

(defvar +from-keywords+ '(:from :upfrom :downfrom))

(defvar +to-keywords+ '(:to :upto :downto :above :below))

(defvar +by-keywords+ '(:by))

(defun parse-for-as-arithmetic/from-to-by (client scope keyword tokens)
  (parse-for-as-arithmetic client scope tokens
                           keyword +to-keywords+ +by-keywords+))

(defun parse-for-as-arithmetic/to-from-by (client scope keyword tokens)
  (parse-for-as-arithmetic client scope tokens
                           keyword +from-keywords+ +by-keywords+))
  
(defun parse-for-as-arithmetic/by-from-to (client scope keyword tokens)
  (parse-for-as-arithmetic client scope tokens
                           keyword +from-keywords+ +to-keywords+))
  
(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :from)) tokens)
  (parse-for-as-arithmetic/from-to-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :upfrom)) tokens)
  (parse-for-as-arithmetic/from-to-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :downfrom)) tokens)
  (parse-for-as-arithmetic/from-to-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :to)) tokens)
  (parse-for-as-arithmetic/to-from-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :upto)) tokens)
  (parse-for-as-arithmetic/to-from-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :downto)) tokens)
  (parse-for-as-arithmetic/to-from-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :above)) tokens)
  (parse-for-as-arithmetic/to-from-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :below)) tokens)
  (parse-for-as-arithmetic/to-from-by client scope keyword tokens))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :by)) tokens)
  (parse-for-as-arithmetic/by-from-to client scope keyword tokens))

;;; FOR-AS-ARITHMETIC expansion methods

(defmethod map-variables (function (clause for-as-arithmetic))
  (map-variables function (var clause)))

(defmethod analyze ((clause for-as-arithmetic))
  (with-accessors ((next-var next-var)
                   (by-var by-var)
                   (var var)
                   (start-form start-form)
                   (by-form by-form)
                   (end-form end-form))
      clause
    (with-accessors ((var-type type-spec))
        var
      (with-accessors ((next-type type-spec))
          next-var
        (with-accessors ((by-type type-spec))
            by-var
          (check-nullable-simple-var-spec (var clause))
          (cond ((not (eq var-type *placeholder-result*))
                 (check-subtype var-type 'number)
                 (setf next-type (numeric-super-type var-type)
                       by-type next-type)
                 (when (numberp start-form)
                   (setf start-form (coerce start-form next-type)))
                 (when (numberp by-form)
                   (setf by-form (coerce by-form by-type))))
                ((numberp start-form)
                 (let ((val start-form))
                   (when (numberp by-form)
                     (incf val by-form))
                   (setf next-type (numeric-type-of val)
                         by-type next-type
                         var-type next-type))
                 (setf start-form (coerce start-form next-type))
                 (when (numberp by-form)
                   (setf by-form (coerce by-form by-type))))
                (t
                 (setf var-type 'number
                       by-type 'number))))))))
 
(defmethod initial-bindings ((clause for-as-arithmetic))
  (nconc (mapcan (lambda (name)
                   (ecase name
                     (:from
                      (d-spec-simple-bindings (next-var clause) (start-form clause)))
                     (:to
                      (when (and (end-form clause)
                                     (not (numberp (end-form clause))))
                        (d-spec-simple-bindings (end-var clause) (end-form clause))))
                     (:by
                      (unless (numberp (by-form clause))
                        (d-spec-simple-bindings (by-var clause) (by-form clause))))))
                 (order clause))
         (d-spec-simple-bindings (var clause))))

(defmethod initial-declarations ((clause for-as-arithmetic))
  (nconc (d-spec-simple-declarations (next-var clause))
         (unless (numberp (by-form clause))
           (d-spec-simple-declarations (by-var clause)))
         (unless (or (null (end-form clause))
                     (numberp (end-form clause)))
           (d-spec-simple-declarations (end-var clause)))
         (d-spec-simple-declarations (var clause) :ignorable t :nullable t)))

(defmethod initial-step-forms ((clause for-as-arithmetic-up))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(var-spec (next-var clause))
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (var-spec (end-var clause))))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defmethod initial-step-forms ((clause for-as-arithmetic-down))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(if (numberp (end-form clause))
                           (end-form clause)
                           (var-spec (end-var clause)))
                      ,(var-spec (next-var clause)))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defmethod subsequent-step-forms ((clause for-as-arithmetic-up))
  (nconc `((incf ,(var-spec (next-var clause))
                 ,(if (numberp (by-form clause))
                      (by-form clause)
                      (var-spec (by-var clause)))))
         (initial-step-forms clause)))

(defmethod subsequent-step-forms ((clause for-as-arithmetic-down))
  (nconc `((decf ,(var-spec (next-var clause))
                 ,(if (numberp (by-form clause))
                      (by-form clause)
                      (var-spec (by-var clause)))))
         (initial-step-forms clause)))

;;; 6.1.2.1.2/6.1.2.1.3 FOR-AS-IN-LIST/FOR-AS-ON-LIST subclauses

(defclass for-as-list (for-as-subclause form-mixin form-var-mixin)
  ((%by-form :accessor by-form
             :initarg :by-form
             :initform '#'cdr)
   (%by-var :reader by-var
            :initform (gensym "BY"))
   (%rest-var :reader rest-var
              :initform (gensym "REST"))))

(defclass for-as-in-list (for-as-list)
  ())

(defclass for-as-on-list (for-as-list)
  ())

;;; FOR-AS-IN-LIST/FOR-AS-ON-LIST parsers

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :in)) tokens)
  (let ((instance (make-instance 'for-as-in-list
                                 :form (pop-token client scope tokens))))
    (when (pop-token? client scope tokens :keywords '(:by))
      (setf (by-form instance) (pop-token client scope tokens)))
    instance))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :on)) tokens)
  (let ((instance (make-instance 'for-as-on-list
                                 :form (pop-token client scope tokens))))
    (when (pop-token? client scope tokens :keywords '(:by))
      (setf (by-form instance) (pop-token client scope tokens)))
    instance))

;;; FOR-AS-IN-LIST/FOR-AS-ON-LIST expansion methods

(defmethod initial-bindings ((clause for-as-list))
  `((,(form-var clause) ,(form clause))
    ,@(unless (function-operator-p (by-form clause))
        `((,(by-var clause) ,(by-form clause))))))

(defmethod final-bindings ((clause for-as-list))
  `((,(rest-var clause) ,(form-var clause))
    ,.(d-spec-outer-bindings (var clause))))

(defmethod final-declarations ((clause for-as-list))
  (d-spec-outer-declarations (var clause)))

(defmethod initial-step-forms ((clause for-as-in-list))
  `((when (endp ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) `(car ,(rest-var clause)))))

(defmethod initial-step-forms ((clause for-as-on-list))
  `((when (atom ,(rest-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause) (rest-var clause))))

(defmethod subsequent-step-forms ((clause for-as-list))
  `((setq ,(rest-var clause)
          ,(if (function-operator-p (by-form clause))
               `(,(second (by-form clause)) ,(rest-var clause))
               `(funcall ,(by-var clause) ,(rest-var clause))))
    ,@(initial-step-forms clause)))

;;; 6.1.2.1.4 FOR-AS-EQUALS-THEN subclause

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :reader initial-form
                  :initarg :initial-form)
   (%subsequent-form :reader subsequent-form
                     :initarg :subsequent-form)))

;;; FOR-AS-EQUALS-THEN parsers

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :=)) tokens)
  (let ((initial-form (pop-token client scope tokens)))
    (make-instance 'for-as-equals-then
                   :initial-form initial-form
                   :subsequent-form (if (pop-token? client scope tokens :keywords '(:then))
                                        (pop-token client scope tokens)
                                        initial-form))))

(defmethod analyze ((clause for-as-equals-then))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (set-d-spec-temps (var clause) t))

;;; FOR-AS-EQUALS-THEN expansion methods

(defmethod initial-bindings ((clause for-as-equals-then))
  (d-spec-outer-bindings (var clause)))

(defmethod initial-declarations ((clause for-as-equals-then))
  (d-spec-outer-declarations (var clause)))

(defmethod initial-step-bindings ((clause for-as-equals-then))
  (d-spec-inner-bindings (var clause) (initial-form clause)))

(defmethod initial-step-forms ((clause for-as-equals-then))
  `((setq ,@(d-spec-inner-assignments (var clause) (initial-form clause)))))

(defmethod subsequent-step-bindings ((clause for-as-equals-then))
  (d-spec-inner-bindings (var clause) (subsequent-form clause)))

(defmethod subsequent-step-forms ((clause for-as-equals-then))
  `((setq ,@(d-spec-inner-assignments (var clause) (subsequent-form clause)))))

;;; 6.1.2.1.5 FOR-AS-ACROSS subclause

(defclass for-as-across (for-as-subclause form-mixin form-var-mixin)
  ((%length-var :reader length-var
                :initform (gensym "LENGTH"))
   (%index-var :reader index-var
               :initform (gensym "INDEX"))))

;;; FOR-AS-ACROSS parsers

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :across)) tokens)
  (make-instance 'for-as-across
                 :form (pop-token client scope tokens)))

;;; FOR-AS-ACROSS expansion methods

(defmethod initial-bindings ((clause for-as-across))
  `((,(form-var clause) ,(form clause))
    (,(index-var clause) 0)))

(defmethod initial-declarations ((clause for-as-across))
  `((type vector ,(form-var clause))
    (type fixnum ,(index-var clause))))
  
(defmethod final-bindings ((clause for-as-across))
  `((,(length-var clause) (length ,(form-var clause)))
    ,.(d-spec-outer-bindings (var clause))))

(defmethod final-declarations ((clause for-as-across))
  (list* `(type fixnum ,(length-var clause))
         (d-spec-outer-declarations (var clause))))

(defmethod initial-step-forms ((clause for-as-across))
  `((when (>= ,(index-var clause) ,(length-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))))

(defmethod subsequent-step-forms ((clause for-as-across))
  `((when (>= (incf ,(index-var clause)) ,(length-var clause))
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         `(aref ,(form-var clause)
                                ,(index-var clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Clause FOR-AS-PATH

(defclass for-as-path (for-as-subclause)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers

(defparameter *current-path* nil)

(defparameter *current-path-prepositions* nil)

(defparameter *current-path-usings* nil)

(defun current-path-preposition-p (name)
  (and *current-path*
       (or (let ((key (symbol-lookup name (path-preposition-names *current-path*))))
             (and key
                  (not (member key *current-path-prepositions*))))
           (and (symbol-equal name :using)
                (path-using-names *current-path*)))
           
       t))

(defun current-path-using-p (name)
  (and *current-path*
       (not (member :using *current-path-prepositions*))
       (let ((key (symbol-lookup name (path-using-names *current-path*))))
         (and key
              (not (member key *current-path-usings*))))
       t))

(defun make-path-iterator-name (client scope token)
  (when (symbolp token)
    (multiple-value-bind (name status)
        (find-symbol (symbol-name token) :keyword)
      (when status
        (return-from make-path-iterator-name name))))
  (error 'unknown-path-iterator
         :client client
         :scope scope
         :name token))

(defmethod make-path-iterator (client scope name &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (error 'unknown-path-iterator
         :client client
         :scope scope
         :name token
         :inclusive inclusive-form-p))

(defmethod parse-tokens ((client standard-client) (scope for-as-clause) (keyword (eql :being)) tokens)
  (prog ((*current-path*
          (if (pop-token? client scope tokens :keywords '(:each :the))
              (make-path-iterator client scope
                                  (make-path-iterator-name client scope
                                                           (pop-token client scope tokens)))
              (let ((form (pop-token client scope tokens)))
                (pop-token client scope tokens :keywords '(:and))
                (pop-token client scope tokens :keywords '(:its :each :his :her))
                (make-path-iterator client scope
                                    (make-path-iterator-name client scope
                                                             (pop-token client scope tokens))
                                    form))))
         (*current-path-prepositions* nil)
         (*current-path-usings* nil)
         (using nil)
         (name nil))
   next-preposition
     (multiple-value-bind (foundp token)
         (pop-token? client scope tokens :type '(satisfies current-path-preposition-p))
       (unless foundp
         (return *current-path*))
       (unless (symbol-equal token :using)
         (setf name (symbol-lookup token
                                   (path-preposition-names *current-path*))
               (path-preposition *current-path* name)
               (pop-token client *current-path* tokens))
         (push name *current-path-prepositions*)
         (go next-preposition)))
     (setf using (pop-token client scope tokens :type 'cons))
   next-using
     (unless (current-path-using-p (car using))
       (error "fu"))
     (setf name (symbol-lookup (car using)
                               (path-using-names *current-path*))
           (path-using *current-path* name) (cadr using)
           using (cddr using))
     (when using
       (go next-using))
     (go next-preposition)))

;;; 6.1.2.1.6 FOR-AS-HASH Subclause

(defclass for-as-hash (for-as-subclause form-mixin form-var-mixin)
  ((%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-key-var :reader temp-key-var
                  :initform (gensym))
   (%temp-value-var :reader temp-value-var
                    :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%other-var :accessor other-var
               :initarg :other-var
               :initform (make-instance 'd-spec
                                        :var-spec nil))))

(defclass for-as-hash-key (for-as-hash)
  ())

(defclass for-as-hash-value (for-as-hash)
  ())

;;; FOR-AS-HASH path extension support

(defmethod map-variables :after (function (clause for-as-hash))
  (map-variables function (other-var clause)))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :hash-key)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-key)))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :hash-keys)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-key)))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :hash-value)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-value)))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :hash-values)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-value)))

(defmethod path-preposition-names ((instance for-as-hash))
  '((:in . :in) (:of . :in)))

(defmethod (setf path-preposition)
    (expression (instance for-as-hash) (key (eql :in)))
  (setf (form instance) expression))

(defmethod path-using-names ((instance for-as-hash-key))
  '((:hash-value . :other) (:hash-values . :other)))

(defmethod path-using-names ((instance for-as-hash-value))
  '((:hash-key . :other) (:hash-keys . :other)))

(defmethod (setf path-using)
    (value (instance for-as-hash) (key (eql :other)))
  (setf (other-var instance) (make-instance 'd-spec
                                            :var-spec value))
  value)

;;; FOR-AS-HASH expansion methods

(defmethod analyze ((clause for-as-hash))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (unless (slot-boundp clause '%form)
    (error "Missing IN/OF preposition")))

(defmethod initial-bindings ((clause for-as-hash))
  `((,(form-var clause) ,(form clause))))

(defmethod final-bindings ((clause for-as-hash))
  `((,(temp-entry-p-var clause) nil)
    (,(temp-key-var clause) nil)
    (,(temp-value-var clause) nil)
    ,.(d-spec-outer-bindings (var clause))
    ,.(d-spec-outer-bindings (other-var clause))))

(defmethod final-declarations ((clause for-as-hash))
  (d-spec-outer-declarations (var clause)))
  
(defmethod wrap-subclause :around ((subclause for-as-hash) inner-form)
  (call-next-method subclause
                    `((with-hash-table-iterator
                          (,(iterator-var subclause) ,(form-var subclause))
                        ,@inner-form))))

(defmethod initial-step-forms ((clause for-as-hash))
  `((multiple-value-setq (,(temp-entry-p-var clause)
                          ,(temp-key-var clause)
                          ,(temp-value-var clause))
      (,(iterator-var clause)))
    (unless ,(temp-entry-p-var clause)
      (go ,*epilogue-tag*))))

(defmethod initial-step-forms :around ((clause for-as-hash-key))
  (nconc (call-next-method)
         (d-spec-inner-form (var clause)
                            (temp-key-var clause))
         (d-spec-inner-form (other-var clause)
                            (temp-value-var clause))))

(defmethod initial-step-forms :around ((clause for-as-hash-value))
  (nconc (call-next-method)
         (d-spec-inner-form (var clause)
                            (temp-value-var clause))
         (d-spec-inner-form (other-var clause)
                            (temp-key-var clause))))

(defmethod subsequent-step-forms ((clause for-as-hash))
  (initial-step-forms clause))

;;; 6.1.2.1.7 FOR-AS-PACKAGE Subclause

(defclass for-as-package (for-as-subclause form-mixin form-var-mixin)
  ((%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-symbol-var :reader temp-symbol-var
                     :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form '*package*))

;;; FOR-AS-PACKAGE path protocol support

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :symbol)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                      :iterator-keywords '(:internal :external :inherited))))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :symbols)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                      :iterator-keywords '(:internal :external :inherited))))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :present-symbol)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                      :iterator-keywords '(:internal :external))))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :present-symbols)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                      :iterator-keywords '(:internal :external))))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :external-symbol)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                      :iterator-keywords '(:external))))

(defmethod make-path-iterator
    ((client standard-client) scope (name (eql :external-symbols)) &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore scope inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                      :iterator-keywords '(:external))))

(defmethod path-preposition-names ((instance for-as-package))
  '((:in . :in) (:of . :in)))

(defmethod (setf path-preposition)
    (expression (instance for-as-package) (key (eql :in)))
  (setf (form instance) expression))

;;; FOR-AS-PACKAGE expansion methods

(defmethod analyze ((clause for-as-package))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t)))

(defmethod initial-bindings ((clause for-as-package))
  `((,(form-var clause) ,(form clause))))

(defmethod final-bindings ((clause for-as-package))
  `((,(temp-entry-p-var clause) nil)
    (,(temp-symbol-var clause) nil)
    ,.(d-spec-outer-bindings (var clause))))

(defmethod final-declarations ((clause for-as-package))
  (d-spec-outer-declarations (var clause)))
  
(defmethod wrap-subclause :around ((subclause for-as-package) inner-form)
  (call-next-method subclause
                    `((with-package-iterator
                          (,(iterator-var subclause)
                            ,(form-var subclause)
                            ,@(iterator-keywords subclause))
                        ,@inner-form))))

(defmethod initial-step-forms ((clause for-as-package))
  `((multiple-value-setq (,(temp-entry-p-var clause)
                          ,(temp-symbol-var clause))
      (,(iterator-var clause)))
    (unless ,(temp-entry-p-var clause)
      (go ,*epilogue-tag*))
    ,@(d-spec-inner-form (var clause)
                         (temp-symbol-var clause))))

(defmethod subsequent-step-forms ((clause for-as-package))
  (initial-step-forms clause))

;;; 6.1.2.2 WITH clause
;;;
;;; A WITH clause allows the creation of local variables.  It is
;;; executed once.
;;;
;;; The syntax of a with-clause is:
;;;
;;;    with-clause ::= WITH var1 [type-spec] [= form1]
;;;                    {AND var2 [type-spec] [= form2]}*
;;;
;;; where var1 and var2 are destructuring variable specifiers
;;; (d-var-spec) allowing multiple local variables to be created in a
;;; single with-clause by destructuring the value of the corresponding
;;; form.
;;;
;;; When there are several consecutive with-clause, the execution is
;;; done sequentially, so that variables created in one with-clause
;;; can be used in the forms of subsequent with-clauses.  If parallel
;;; creation of variables is wanted, then the with-clause can be
;;; followed by one or more and-clauses.
;;;
;;; The (destructuring) type specifier is optional.  If no type
;;; specifier is given, it is as if t was given.
;;;
;;; The initialization form is optional.  If there is a corresponding
;;; type specifier for a variable, but no initialization form, then
;;; the variable is initialized to a value that is appropriate for the
;;; type.  In particular, for the type t the value is nil, for the
;;; type number, the value is 0, and for the type float, the value is
;;; 0.0.

(defclass with-clause (variable-clause subclauses-mixin)
  ())

(defclass with-subclause (var-mixin)
  ())

(defclass with-subclause-no-form (with-subclause)
  ())

(defclass with-subclause-with-form
    (with-subclause form-mixin form-var-mixin)
  ())

;;; WITH Parsers

(defmethod parse-tokens ((client standard-client) (scope body-clauses) (keyword (eql :with)) tokens)
  (prog ((instance (make-instance 'with-clause))
         var
         subclauses)
   next
     (setf var (parse-d-spec client scope tokens))
     (if (pop-token? client instance tokens :keywords '(:=))
         (push (make-instance 'with-subclause-with-form
                              :var var
                              :form (pop-token client scope tokens))
               subclauses)
         (push (make-instance 'with-subclause-no-form
                              :var var)
               subclauses))
     (when (pop-token? client instance tokens :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses))
     (return instance)))  

;;; WITH expansion methods

(defmethod map-variables (function (clause with-clause))
  (map-variables function (subclauses clause)))

(defmethod initial-bindings ((clause with-subclause-with-form))
  (list* `(,(form-var clause) ,(form clause))
         (d-spec-outer-bindings (var clause))))

(defmethod initial-bindings ((clause with-subclause-no-form))
  (let ((result '()))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                   (push `(,var ,(if (subtypep type 'number)
                               (coerce 0 type)
                               nil)) result))
                   (var clause))
    (nreverse result)))

(defmethod wrap-subclause ((subclause with-subclause-with-form) inner-form)
  (nconc (d-spec-inner-form (var subclause) (form-var subclause))
         inner-form))

(defmethod initial-declarations ((clause with-subclause))
  (d-spec-outer-declarations (var clause)))
