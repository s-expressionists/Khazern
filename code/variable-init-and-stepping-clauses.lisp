(cl:in-package #:khazern)

;;; 6.1.2.1 FOR-AS-CLAUSE clause
;;;
;;; The ANSI specification says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;   for-as-clause    ::= {FOR | AS} for-as-subclause {and for-as-subclause}* 
;;;   for-as-subclause ::= for-as-arithmetic | for-as-in-list | for-as-on-list |
;;;                        for-as-equals-then | for-as-across | for-as-hash | for-as-package 
;;;
;;; Since Khazern supports for-as-hash and for-as-package via iteration path extensions the
;;; grammar is modified:
;;;
;;;   for-as-clause    ::= {FOR | AS} for-as-subclause {and for-as-subclause}* 
;;;   for-as-subclause ::= for-as-arithmetic | for-as-in-list | for-as-on-list |
;;;                        for-as-equals-then | for-as-across | for-as-iteration-path

(defclass for-as-clause (binding-clause parallel-superclause)
  ())

(defclass for-as-subclause (clause var-mixin)
  ())

(defun parse-for-as (client)
  (prog ((instance (make-instance 'for-as-clause
                                  :start *start*))
         subclauses
         var
         start)
   next
     (setf start *index*
           var (parse-d-spec :type-spec *placeholder-result*)
           (ignorablep var) t)
     (push (do-parse-clause client instance start)
           subclauses)
     (setf (var (car subclauses)) var)
     (when (pop-token? :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses)
           (end instance) *index*)
     (return instance)))

(defmethod parse-clause
    ((client standard-client) (scope extended-superclause) (keyword (eql :for)))
  (parse-for-as client))

(defmethod parse-clause
    ((client standard-client) (scope extended-superclause) (keyword (eql :as)))
  (parse-for-as client))

(defmethod analyze ((clause for-as-subclause))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

;;; FOR-AS-ITERATION-PATH subclause
;;;
;;; Grammar:
;;;
;;;   for-as-iteration-path ::= BEING {path-exclusive | path-inclusive}
;;;                             {path-using | path-preposition}*
;;;   path-exclusive        ::= {EACH | THE} path
;;;   path-inclusive        ::= form AND {ITS | EACH | HIS | HER} path
;;;   path-using            ::= USING ({simple-var simple-var}+)
;;;   path-preposition      ::= name form

(defclass for-as-iteration-path (for-as-subclause)
  ((%preposition-names :accessor iteration-path-preposition-names
                       :initarg :preposition-names
                       :initform nil)
   (%using-names :accessor iteration-path-using-names
                 :initarg :using-names
                 :initform nil)))

(defun make-iteration-path-name (client token)
  (when (symbolp token)
    (multiple-value-bind (name status)
        (find-symbol (symbol-name token) :keyword)
      (when status
        (return-from make-iteration-path-name name))))
  (error 'unknown-iteration-path
         :client client
         :name token))

(defmethod make-iteration-path (client name &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (error 'unknown-iteration-path
         :client client
         :name name
         :inclusive inclusive-form-p))

(defun parse-iteration-path-using (instance using)
  (prog (key value name)
   next-using
     (setf key (first using)
           value (second using) 
           name (find key (iteration-path-using-names instance)
                      :test #'symbol-equal))
     (when (null name)
       (let ((keywords (iteration-path-using-names instance)))
         (if keywords
             (error 'expected-token-but-found
                    :found key
                    :expected-keywords keywords)
             (error 'unexpected-token-found
                    :found key))))
     (setf (iteration-path-using instance name) value
           using (cddr using))
     (when using
       (go next-using))))

(defun parse-iteration-path-prepositions (instance)
  (prog ((name nil)
         (foundp nil)
         (token nil)
         (usingp (and (iteration-path-using-names instance) t)))
   next-preposition
     (multiple-value-setq (foundp token)
       (pop-token? :keywords (if usingp
                                 (list* :using
                                        (iteration-path-preposition-names instance))
                                 (iteration-path-preposition-names instance))))
     (cond ((not foundp)
            (return instance))
           ((symbol-equal token :using)
            (setf usingp nil)
            (parse-iteration-path-using instance (pop-token :type 'cons)))
           (t
            (setf name (find token (iteration-path-preposition-names instance)
                             :test #'symbol-equal)
                  (iteration-path-preposition instance name) (pop-token))))
     (go next-preposition)))

(defmethod parse-clause
    ((client standard-client) (instance for-as-clause) (keyword (eql :being)))
  (let ((instance (if (pop-token? :keywords '(:each :the))
                      (make-iteration-path client
                                           (make-iteration-path-name client
                                                                     (pop-token)))
                      (let ((form (pop-token)))
                        (pop-token :keywords '(:and))
                        (pop-token :keywords '(:its :each :his :her))
                        (make-iteration-path client
                                             (make-iteration-path-name client
                                                                       (pop-token))
                                             form)))))
    (parse-iteration-path-prepositions instance)
    instance))

;;; 6.1.2.1.1 FOR-AS-ARITHMETIC subclause
;;;
;;; Grammar:
;;;
;;;   for-as-arithmetic           ::= var [type-spec] for-as-arithmetic-subclause
;;;   for-as-arithmetic-subclause ::= arithmetic-up | arithmetic-downto | arithmetic-downfrom
;;;   arithmetic-up               ::= ⟦{FROM | UPFROM} form1 | {TO | UPTO | BELOW} form2 |
;;;                                    BY form3⟧+
;;;   arithmetic-downto           ::= ⟦{FROM form1} | {{DOWNTO | ABOVE} form2}1 | BY form3⟧
;;;   arithmetic-downfrom         ::= ⟦{DOWNFROM form1} | {TO | DOWNTO | ABOVE} form2 |
;;;                                    BY form3⟧

(defclass for-as-arithmetic (for-as-iteration-path)
  ((%next-ref :accessor next-ref
              :initform nil)
   (%next-var :accessor next-var
              :initform nil)
   (%end-ref :accessor end-ref
             :initform nil)
   (%end-var :accessor end-var
            :initform nil)
   (%by-ref :accessor by-ref
            :initform 1)
   (%by-var :accessor by-var
            :initform nil)
   (%numeric-value :accessor numeric-value
                   :initform nil)
   ;; If termination is TO, UPTO, or DOWNTO, then this slot contains
   ;; the symbol <=.  If termination is ABOVE or BELOW, then this slot
   ;; contains the symbol <.  If there is TO/UPTO/DOWNTO/ABOVE/BELOW,
   ;; then the loop does not terminate because of this clause, and
   ;; then this slot contains NIL.
   (%termination-test :accessor termination-test
                      :initarg :termination-test
                      :initform nil))
  (:default-initargs :preposition-names (list :from :upfrom :downfrom
                                              :to :upto :downto :above
                                              :below :by)))

(defclass for-as-arithmetic-up (for-as-arithmetic)
  ())

(defclass for-as-arithmetic-down (for-as-arithmetic)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FOR-AS-ARITHMETIC parsers

(defvar +from-keywords+ '(:from :upfrom :downfrom))

(defvar +to-keywords+ '(:to :upto :downto :above :below))

(defvar +by-keywords+ '(:by))

(defmethod (setf iteration-path-preposition) (value (instance for-as-arithmetic) name)
  ;; parse the form
  (ecase name
    ((:to :upto :downto :above :below)
     (setf (values (end-ref instance) (end-var instance))
               (add-binding instance :var (gensym "END") :type 'number :form value :fold t)
           (iteration-path-preposition-names instance)
               (nset-difference (iteration-path-preposition-names instance)
                                +to-keywords+)))
    ((:from :downfrom :upfrom)
     (setf (values (next-ref instance) (next-var instance))
               (add-binding instance :var (gensym "NEXT") :type 'number :form value)
           (iteration-path-preposition-names instance)
               (nset-difference (iteration-path-preposition-names instance)
                                +from-keywords+)))
    (:by
     (setf (values (by-ref instance) (by-var instance))
               (add-binding instance :var (gensym "BY") :type 'number :form value :fold t)
           (iteration-path-preposition-names instance)
               (delete :by (iteration-path-preposition-names instance)))))
  ;; set the termination test
  (case name
    ((:to :upto :downto)
     (setf (termination-test instance) '<=))
    ((:above :below)
     (setf (termination-test instance) '<)))
  ;; set the direction
  (case name
    ((:upto :upfrom :below)
     (cond ((typep instance 'for-as-arithmetic-down)
            (error 'conflicting-stepping-directions
                   :clause (subseq *body* *start* *index*)))
           ((not (typep instance 'for-as-arithmetic-up))
            (change-class instance 'for-as-arithmetic-up))))
    ((:downto :downfrom :above)
     (cond ((typep instance 'for-as-arithmetic-up)
            (error 'conflicting-stepping-directions
                   :clause (subseq *body* *start* *index*)))
           ((not (typep instance 'for-as-arithmetic-down))
            (change-class instance 'for-as-arithmetic-down)))))
  nil)

(defun parse-for-as-arithmetic (keyword)
  (let ((instance (make-instance 'for-as-arithmetic
                                 :start *start*)))
    (push-token keyword)
    (parse-iteration-path-prepositions instance)
    (setf (end instance) *index*)
    instance))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :from)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :upfrom)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause
    ((client standard-client) (scope for-as-clause) (keyword (eql :downfrom)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :to)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :upto)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :downto)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :above)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :below)))
  (parse-for-as-arithmetic keyword))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :by)))
  (parse-for-as-arithmetic keyword))

;;; FOR-AS-ARITHMETIC expansion methods

(defmethod map-variables (function (clause for-as-arithmetic))
  (map-variables function (var clause)))

(defmethod analyze ((clause for-as-arithmetic))
  (unless (typep clause '(or for-as-arithmetic-down for-as-arithmetic-up))
    (change-class clause 'for-as-arithmetic-up))
  (with-accessors ((next-var next-var)
                   (by-var by-var)
                   (var var)
                   (by-ref by-ref))
      clause
    (with-accessors ((var-type type-spec))
        var
      (unless next-var
        (setf (iteration-path-preposition clause :from) 0))
      (with-accessors ((next-type type-spec))
          next-var
        (check-nullable-simple-var-spec (var clause))
        (let ((val nil))
          (cond ((not (eq var-type *placeholder-result*))
                 (check-subtype var-type 'number)
                 (setf next-type (numeric-super-type var-type)))
                ((setf val (find-if #'numberp (forms clause)))
                 (when (numberp by-ref)
                   (incf val by-ref))
                 (setf next-type (numeric-type-of val)
                       var-type next-type))
                (t
                 (setf var-type 'number
                       next-type 'number))))
        (when by-var
          (setf (type-spec by-var) next-type))
        (setf (forms clause) (mapcar (lambda (form)
                                       (if (numberp form)
                                           (coerce form next-type)
                                           form))
                                     (forms clause)))
        (when (numberp by-ref)
          (setf by-ref (coerce by-ref next-type)))
        (check-type-spec var)))))
 
(defmethod initial-bindings nconc ((clause for-as-arithmetic))
  (d-spec-outer-bindings (var clause)))

(defmethod initial-declarations nconc ((clause for-as-arithmetic))
  (d-spec-outer-declarations (var clause)))

(defmethod initial-step-forms ((clause for-as-arithmetic-up))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(next-ref clause)
                      ,(end-ref clause))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defmethod initial-step-forms ((clause for-as-arithmetic-down))
  (nconc (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(end-ref clause)
                      ,(next-ref clause))
               (go ,*epilogue-tag*))))
         (when (var-spec (var clause))
           `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause)))))))

(defmethod subsequent-step-forms ((clause for-as-arithmetic-up))
  (nconc `((incf ,(var-spec (next-var clause)) ,(by-ref clause)))
         (initial-step-forms clause)))

(defmethod subsequent-step-forms ((clause for-as-arithmetic-down))
  (nconc `((decf ,(var-spec (next-var clause)) ,(by-ref clause)))
         (initial-step-forms clause)))

;;; 6.1.2.1.2/6.1.2.1.3 FOR-AS-IN-LIST/FOR-AS-ON-LIST subclauses
;;;
;;; Grammar:
;;;
;;; for-as-in-list ::= var [type-spec] IN form [BY step-fun]
;;; for-as-on-list ::= var [type-spec] ON form [BY step-fun] 

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

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :in)))
  (let ((instance (make-instance 'for-as-in-list
                                 :start *start*
                                 :form (pop-token))))
    (when (pop-token? :keywords '(:by))
      (setf (by-form instance) (pop-token)))
    (setf (end instance) *index*)
    instance))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :on)))
  (let ((instance (make-instance 'for-as-on-list
                                 :start *start*
                                 :form (pop-token))))
    (when (pop-token? :keywords '(:by))
      (setf (by-form instance) (pop-token)))
    (setf (end instance) *index*)
    instance))

(defmethod initial-bindings nconc ((clause for-as-list))
  `((,(rest-var clause) ,(form clause))
    ,@(unless (function-operator-p (by-form clause))
        `((,(by-var clause) ,(by-form clause))))
    ,.(d-spec-outer-bindings (var clause))))

(defmethod initial-declarations nconc ((clause for-as-list))
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
;;;
;;; Grammar:
;;;
;;;   for-as-equals-then := var [type-spec] = form [THEN form2]

(defclass for-as-equals-then (for-as-subclause)
  ((%initial-form :reader initial-form
                  :initarg :initial-form)
   (%subsequent-form :reader subsequent-form
                     :initarg :subsequent-form)))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :=)))
  (let ((initial-form (pop-token)))
    (make-instance 'for-as-equals-then
                   :start *start*
                   :initial-form initial-form
                   :subsequent-form (if (pop-token? :keywords '(:then))
                                        (pop-token)
                                        initial-form)
                   :end *index*)))

(defmethod analyze ((clause for-as-equals-then))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (set-d-spec-temps (var clause) t)
  (check-type-spec (var clause)))

(defmethod initial-bindings nconc ((clause for-as-equals-then))
  (d-spec-outer-bindings (var clause)))

(defmethod initial-declarations nconc ((clause for-as-equals-then))
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
;;;
;;; Grammar:
;;;
;;;  for-as-across ::= var [type-spec] ACROSS vector

(defclass for-as-across (for-as-subclause form-mixin form-var-mixin)
  ((%length-var :reader length-var
                :initform (gensym "LENGTH"))
   (%index-var :reader index-var
               :initform (gensym "INDEX"))))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :across)))
  (make-instance 'for-as-across
                 :start *start*
                 :form (pop-token)
                 :end *index*))

(defmethod initial-bindings nconc ((clause for-as-across))
  `((,(form-var clause) ,(form clause))
    (,(index-var clause) 0)
    (,(length-var clause) 0)
    ,.(d-spec-outer-bindings (var clause))))

(defmethod initial-declarations nconc ((clause for-as-across))
  `((type vector ,(form-var clause))
    (type fixnum ,(index-var clause))
    (type fixnum ,(length-var clause))
    ,.(d-spec-outer-declarations (var clause))))

(defmethod initial-step-forms ((clause for-as-across))
  `((setq ,(length-var clause) (length ,(form-var clause)))
    (when (>= ,(index-var clause) ,(length-var clause))
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

;;; 6.1.2.1.6 FOR-AS-HASH Subclause
;;;
;;; Grammar:
;;;
;;;   for-as-hash ::= var [type-spec] BEING {EACH | THE} {hash-key | hash-value}
;;;   hash-key    ::= {HASH-KEY | HASH-KEYS} {IN | OF} hash-table
;;;                   [USING (HASH-VALUE other-var)]
;;;   hash-value  ::= {HASH-VALUE | HASH-VALUES} {IN | OF} hash-table
;;;                   [USING (HASH-KEY other-var)]
;;;
;;; FOR-AS-HASH is implemented as either the HASH-KEY/HASH-KEYS iteration path with preposition
;;; IN/OF and using HASh-VALUE, or the HASH-VALUE/HASH-VALUES iteration path with preposition
;;; IN/OF and using HASh-KEY. This means that Khazern permits the USING preposition to occur
;;; before IN/OF. In this case a STYLE-WARNING is signalled since the ANSI specification does
;;; not permit such an ordering.

(defclass for-as-hash (for-as-iteration-path form-mixin form-var-mixin)
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
                                        :var-spec nil)))
  (:default-initargs :preposition-names (list :in :of)))

(defclass for-as-hash-key (for-as-hash)
  ()
  (:default-initargs :using-names (list :hash-value)))

(defclass for-as-hash-value (for-as-hash)
  ()
  (:default-initargs :using-names (list :hash-key)))

(defmethod map-variables :after (function (clause for-as-hash))
  (map-variables function (other-var clause)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-key))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-key
                     :start *start*)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-keys))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-key
                     :start *start*)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-value))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-value
                     :start *start*)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-values))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-value
                     :start *start*)))

(defmethod (setf iteration-path-preposition) (expression (instance for-as-hash) key)
  (when (var-spec (other-var instance))
    (warn 'invalid-iteration-path-preposition-order
          :first-preposition :using
          :second-preposition :in
          :name (if (typep instance 'for-as-hash-key)
                    :hash-key
                    :hash-value)))
  (setf (iteration-path-preposition-names instance) nil)
  (setf (form instance) expression))

(defmethod (setf iteration-path-using) (value (instance for-as-hash) key)
  (setf (iteration-path-using-names instance) nil)
  (setf (other-var instance) (make-instance 'd-spec
                                            :var-spec value))
  value)

(defmethod analyze ((clause for-as-hash))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (unless (slot-boundp clause '%form)
    (error 'missing-iteration-path-prepositions
           :names '(:in)
           :name (if (typep clause 'for-as-hash-key)
                     :hash-key
                     :hash-value)))
  (check-type-spec (var clause)))

(defmethod initial-bindings nconc ((clause for-as-hash))
  `((,(form-var clause) ,(form clause))
    (,(temp-entry-p-var clause) nil)
    (,(temp-key-var clause) nil)
    (,(temp-value-var clause) nil)
    ,.(d-spec-outer-bindings (var clause))
    ,.(d-spec-outer-bindings (other-var clause))))

(defmethod initial-declarations nconc ((clause for-as-hash))
  (d-spec-outer-declarations (var clause)))
  
(defmethod wrap-forms ((subclause for-as-hash) forms)
  `((with-hash-table-iterator
        (,(iterator-var subclause) ,(form-var subclause))
      ,@forms)))

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
;;;
;;; Grammar:
;;;
;;; for-as-package ::= var [type-spec] BEING {EACH | THE}
;;;                    {SYMBOL | SYMBOLS | PRESENT-SYMBOL | PRESENT-SYMBOLS | EXTERNAL-SYMBOL |
;;;                     EXTERNAL-SYMBOLS} [{IN | OF} package]
;;;
;;; FOR-AS-PACKAGE is implmented as the SYMBOL(S)/PRESENT-SYMBOL(S)/EXTERNAL-SYMBOL(S) iteration
;;; path extension.

(defclass for-as-package (for-as-iteration-path form-mixin form-var-mixin)
  ((%temp-entry-p-var :reader temp-entry-p-var
                      :initform (gensym))
   (%temp-symbol-var :reader temp-symbol-var
                     :initform (gensym))
   (%iterator-var :reader iterator-var
                  :initform (gensym))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form '*package*
                     :preposition-names (list :in :of)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :symbol))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :iterator-keywords '(:internal :external :inherited))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :symbols))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :iterator-keywords '(:internal :external :inherited))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :present-symbol))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :iterator-keywords '(:internal :external))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :present-symbols))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :iterator-keywords '(:internal :external))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :external-symbol))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :iterator-keywords '(:external))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :external-symbols))
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :iterator-keywords '(:external))))

(defmethod (setf iteration-path-preposition)
    (expression (instance for-as-package) key)
  (setf (iteration-path-preposition-names instance) nil)
  (setf (form instance) expression))

(defmethod analyze ((clause for-as-package))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

(defmethod initial-bindings nconc ((clause for-as-package))
  `((,(form-var clause) ,(form clause))
    (,(temp-entry-p-var clause) nil)
    (,(temp-symbol-var clause) nil)
    ,.(d-spec-outer-bindings (var clause))))

(defmethod initial-declarations nconc ((clause for-as-package))
  (d-spec-outer-declarations (var clause)))
  
(defmethod wrap-forms ((subclause for-as-package) forms)
  `((with-package-iterator
        (,(iterator-var subclause)
         ,(form-var subclause)
         ,@(iterator-keywords subclause))
      ,@forms)))

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

(defclass with-clause (binding-clause parallel-superclause)
  ())

(defclass with-subclause (clause var-mixin)
  ())

(defclass with-subclause-no-form (with-subclause)
  ())

(defclass with-subclause-with-form
    (with-subclause form-mixin form-var-mixin)
  ())

;;; WITH Parsers

(defmethod parse-clause
    ((client standard-client) (scope extended-superclause) (keyword (eql :with)))
  (prog ((instance (make-instance 'with-clause
                                  :start *start*))
         var
         subclauses
         *start*)
   next
     (setf *start* *index*
           var (parse-d-spec)
           (ignorablep var) t)
     (if (pop-token? :keywords '(:=))
         (push (make-instance 'with-subclause-with-form
                              :start *start*
                              :var var
                              :form (pop-token)
                              :end *index*)
               subclauses)
         (push (make-instance 'with-subclause-no-form
                              :start *start*
                              :var var
                              :end *index*)
               subclauses))
     (when (pop-token? :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses)
           (end instance) *index*)
     (return instance)))  

(defmethod analyze ((instance with-subclause))
  (check-type-spec (var instance)))

(defmethod initial-bindings nconc ((clause with-subclause-with-form))
  (list* `(,(form-var clause) ,(form clause))
         (d-spec-outer-bindings (var clause))))

(defmethod initial-bindings nconc ((clause with-subclause-no-form))
  (d-spec-outer-bindings (var clause)))

(defmethod wrap-forms ((subclause with-subclause-with-form) forms)
  (nconc (d-spec-inner-form (var subclause) (form-var subclause))
         forms))

(defmethod initial-declarations nconc ((clause with-subclause))
  (d-spec-outer-declarations (var clause)))
