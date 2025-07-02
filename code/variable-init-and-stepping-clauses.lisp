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
  (prog ((instance (make-instance 'for-as-clause :start *start*))
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
   (%required-preposition-names :accessor iteration-path-required-preposition-names
                                :initarg :required-preposition-names
                                :initform nil)
   (%using-names :accessor iteration-path-using-names
                 :initarg :using-names
                 :initform nil)))
   

(defmethod (setf iteration-path-preposition) :after
    (value (instance for-as-iteration-path) name)
  (declare (ignore name))
  (setf (iteration-path-preposition-names instance)
        (delete-if (lambda (keyword-or-keywords)
                     (find-keyword name keyword-or-keywords))
                   (iteration-path-preposition-names instance))
        (iteration-path-required-preposition-names instance)
        (delete-if (lambda (keyword-or-keywords)
                     (find-keyword name keyword-or-keywords))
                   (iteration-path-required-preposition-names instance))))

(defmethod (setf iteration-path-using) :after
    (value (instance for-as-iteration-path) name)
  (declare (ignore name))
  (setf (iteration-path-using-names instance)
        (delete-if (lambda (keyword-or-keywords)
                     (find-keyword name keyword-or-keywords))
                   (iteration-path-using-names instance))))

(defun make-iteration-path-name (client token &optional inclusive-form-p)
  (when (symbolp token)
    (multiple-value-bind (name status)
        (find-symbol (symbol-name token) :keyword)
      (when status
        (return-from make-iteration-path-name name))))
  (error 'unknown-iteration-path
         :client client
         :name token
         :inclusive inclusive-form-p
         :clause (when (numberp *start*)
                   (subseq *body* *start* *index*))))

(defmethod make-iteration-path (client name &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (error 'unknown-iteration-path
         :client client
         :name name
         :inclusive inclusive-form-p
         :clause (when (numberp *start*)
                   (subseq *body* *start* *index*))))

(defun parse-iteration-path-using (instance using)
  (trivial-with-current-source-form:with-current-source-form (using)
    (prog ((*tokens* using)
           (*toplevel* nil)
           (names nil))
     next-using
       (setf names (iteration-path-using-names instance))
       (unless names
         (error 'unexpected-token-found
                :found (car *tokens*)
                :clause (subseq *body* *start* *index*)))
       (setf (iteration-path-using instance (nth-value 1 (pop-token :keywords names)))
             (pop-token :type 'd-var-spec))
     (when *tokens*
       (go next-using)))))

(defun parse-iteration-path-prepositions (instance)
  (prog ((foundp nil)
         (token nil)
         (keyword nil)
         (names nil)
         (usingp (and (iteration-path-using-names instance) t)))
   next-preposition
     (setf names (if usingp
                     (list* :using
                            (iteration-path-preposition-names instance))
                     (iteration-path-preposition-names instance)))
     (when names
       (cond ((iteration-path-required-preposition-names instance)
              (multiple-value-setq (token keyword)
                (pop-token :keywords names))
              (setf foundp t))
             (t
              (multiple-value-setq (foundp token keyword)
                (pop-token? :keywords names))))
       (cond ((not foundp)
              (return instance))
             ((eq keyword :using)
              (setf usingp nil)
              (parse-iteration-path-using instance (pop-token :type 'cons)))
             (t
              (setf (iteration-path-preposition instance keyword) (pop-token))))
       (go next-preposition))))

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
                                                                       (pop-token) t)
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
  (:default-initargs :preposition-names (list '(:from :upfrom :downfrom)
                                              '(:to :upto :downto :above :below)
                                              :by)))

(defclass for-as-arithmetic-up (for-as-arithmetic)
  ())

(defclass for-as-arithmetic-down (for-as-arithmetic)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FOR-AS-ARITHMETIC parsers

(defmethod (setf iteration-path-preposition) (value (instance for-as-arithmetic) name)
  ;; parse the form
  (ecase name
    ((:to :upto :downto :above :below)
     (setf (values (end-ref instance) (end-var instance))
           (add-simple-binding instance :var "END" :type 'number :form value :fold t)))
    ((:from :downfrom :upfrom)
     (setf (values (next-ref instance) (next-var instance))
           (add-simple-binding instance :var "NEXT" :type 'number :form value)))
    (:by
     (setf (values (by-ref instance) (by-var instance))
           (add-simple-binding instance :var "BY" :type 'number :form value :fold t))))
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
      (with-accessors ((next-type type-spec)
                       (next-form form))
          next-var
        (check-nullable-simple-var-spec (var clause))
        (let ((val nil))
          (cond ((not (eq var-type *placeholder-result*))
                 (check-subtype var-type 'number)
                 (setf next-type (numeric-super-type var-type)))
                ((numberp next-form)
                 (setf val next-form)
                 (when (numberp by-ref)
                   (incf val by-ref))
                 (setf next-type (numeric-type-of val)
                       var-type next-type))
                (t
                 (setf var-type 'number
                       next-type 'number))))
        (when by-var
          (setf (type-spec by-var) next-type))
        (when (numberp next-form)
          (setf next-form (coerce next-form next-type)))
        (when (numberp by-ref)
          (setf by-ref (coerce by-ref next-type)))
        (check-type-spec var)))))

(defmethod step-intro-forms ((clause for-as-arithmetic-up) initialp)
  (nconc (unless initialp
           `((incf ,(var-spec (next-var clause)) ,(by-ref clause))))
         (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(next-ref clause)
                      ,(end-ref clause))
               (go ,*epilogue-tag*))))))

(defmethod step-intro-forms ((clause for-as-arithmetic-down) initialp)
  (nconc (unless initialp
           `((decf ,(var-spec (next-var clause)) ,(by-ref clause))))
         (when (termination-test clause)
           `((unless (,(termination-test clause)
                      ,(end-ref clause)
                      ,(next-ref clause))
               (go ,*epilogue-tag*))))))

(defmethod step-outro-forms ((clause for-as-arithmetic) initialp)
  (declare (ignore initialp))
  (when (var-spec (var clause))
    `((setq ,(var-spec (var clause)) ,(var-spec (next-var clause))))))

;;; 6.1.2.1.2/6.1.2.1.3 FOR-AS-IN-LIST/FOR-AS-ON-LIST subclauses
;;;
;;; Grammar:
;;;
;;; for-as-in-list ::= var [type-spec] IN form [BY step-fun]
;;; for-as-on-list ::= var [type-spec] ON form [BY step-fun] 

(defclass for-as-list (for-as-subclause)
  ((%by-ref :accessor by-ref
            :initform '#'cdr)
   (%rest-var :accessor rest-var)))

(defclass for-as-in-list (for-as-list)
  ())

(defclass for-as-on-list (for-as-list)
  ())

(defun parse-for-as-list (instance)
  (setf (rest-var instance) (add-simple-binding instance :var "REST" :form (pop-token)))
  (when (pop-token? :keywords '(:by))
    (setf (by-ref instance) (add-simple-binding instance :var "BY" :form (pop-token) :fold t
                                                         :fold-test 'function-operator-p)))
  (setf (end instance) *index*)
  instance)

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :in)))
  (parse-for-as-list (make-instance 'for-as-in-list :start *start*)))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :on)))
  (parse-for-as-list (make-instance 'for-as-on-list :start *start*)))

(defmethod step-intro-forms ((clause for-as-list) initialp)
  (unless initialp
    `((setq ,(rest-var clause)
            ,(if (function-operator-p (by-ref clause))
                 `(,(second (by-ref clause)) ,(rest-var clause))
                 `(funcall ,(by-ref clause) ,(rest-var clause)))))))

(defmethod step-intro-forms :around ((clause for-as-in-list) initialp)
  (declare (ignore initialp))
  (nconc (call-next-method)
         `((when (endp ,(rest-var clause))
             (go ,*epilogue-tag*)))))

(defmethod step-outro-forms ((clause for-as-in-list) initialp)
  (declare (ignore initialp))
  (destructuring-set (var clause) `(car ,(rest-var clause))))

(defmethod step-intro-forms :around ((clause for-as-on-list) initialp)
  (declare (ignore initialp))
  (nconc (call-next-method)
         `((when (atom ,(rest-var clause))
             (go ,*epilogue-tag*)))))

(defmethod step-outro-forms ((clause for-as-on-list) initialp)
  (declare (ignore initialp))
  (destructuring-set (var clause) (rest-var clause)))

;;; 6.1.2.1.4 FOR-AS-EQUALS-THEN subclause
;;;
;;; Grammar:
;;;
;;;   for-as-equals-then := var [type-spec] = form [THEN form2]

(defclass for-as-equals-then (for-as-subclause)
  ((%temp-ref :accessor temp-ref)
   (%initial-form :reader initial-form
                  :initarg :initial-form)
   (%subsequent-form :reader subsequent-form
                     :initarg :subsequent-form)))

(defmethod initialize-instance :after ((instance for-as-equals-then) &rest initargs &key)
  (declare (ignore initargs))
  (setf (temp-ref instance) (add-simple-binding instance :var "TMP")))

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
  (check-type-spec (var clause)))

(defmethod step-intro-forms ((clause for-as-equals-then) initialp)
  `((setq ,(temp-ref clause) ,(if initialp
                                  (initial-form clause)
                                  (subsequent-form clause)))))

(defmethod step-outro-forms ((clause for-as-equals-then) initialp)
  (destructuring-set (var clause) (temp-ref clause)))

;;; 6.1.2.1.5 FOR-AS-ACROSS subclause
;;;
;;; Grammar:
;;;
;;;  for-as-across ::= var [type-spec] ACROSS vector

(defclass for-as-across (for-as-subclause form-ref-mixin)
  ((%length-ref :accessor length-ref)
   (%index-ref :accessor index-ref)))

(defmethod initialize-instance :after ((instance for-as-across) &rest initargs &key)
  (declare (ignore initargs))
  (setf (length-ref instance) (add-simple-binding instance :var "LENGTH" :form 0
                                                           :type 'fixnum)
        (index-ref instance) (add-simple-binding instance :var "INDEX" :form 0
                                                          :type 'fixnum)))

(defmethod parse-clause ((client standard-client) (scope for-as-clause) (keyword (eql :across)))
  (let ((instance (make-instance 'for-as-across
                                 :start *start*)))
    (setf (form-ref instance) (add-simple-binding instance :var "VECTOR" :form (pop-token)
                                                           :type 'vector)
          (end instance) *index*)
    instance))

(defmethod step-intro-forms ((clause for-as-across) initialp)
  `(,(if initialp
         `(setq ,(length-ref clause) (length ,(form-ref clause)))
         `(incf ,(index-ref clause)))
    (when (>= ,(index-ref clause) ,(length-ref clause))
      (go ,*epilogue-tag*))))

(defmethod step-outro-forms ((clause for-as-across) initialp)
  (declare (ignore initialp))
  (destructuring-set (var clause) `(aref ,(form-ref clause) ,(index-ref clause))))

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

(defclass for-as-hash (for-as-iteration-path form-ref-mixin other-var-mixin)
  ((%temp-entry-p-var :accessor temp-entry-p-var)
   (%temp-key-var :accessor temp-key-var)
   (%temp-value-var :accessor temp-value-var)
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITER")))
  (:default-initargs :preposition-names (list '(:in :of))
                     :required-preposition-names (list '(:in :of))
                     :other-var (make-instance 'destructuring-binding
                                               :var-spec nil)))

(defmethod initialize-instance :after ((instance for-as-hash) &rest initargs &key)
  (declare (ignore initargs))
  (setf (temp-entry-p-var instance) (add-simple-binding instance :var "ENTRYP")
        (temp-key-var instance) (add-simple-binding instance :var "KEY")
        (temp-value-var instance) (add-simple-binding instance :var "VALUE")))

(defclass for-as-hash-key (for-as-hash)
  ()
  (:default-initargs :using-names (list :hash-value)))

(defclass for-as-hash-value (for-as-hash)
  ()
  (:default-initargs :using-names (list :hash-key)))

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
                    :hash-value)
          :clause (when (numberp *start*)
                    (subseq *body* *start* *index*))))
  (setf (form-ref instance) (add-simple-binding instance :var "HT" :form expression
                                                         :type 'hash-table))
  expression)

(defmethod (setf iteration-path-using) (value (instance for-as-hash) key)
  (setf (other-var instance) (make-instance 'destructuring-binding
                                            :var-spec value))
  value)

(defmethod analyze ((clause for-as-hash))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

(defmethod wrap-forms ((subclause for-as-hash) forms)
  `((with-hash-table-iterator
        (,(iterator-var subclause) ,(form-ref subclause))
      ,@forms)))

(defmethod step-intro-forms ((clause for-as-hash) initialp)
  (declare (ignore initialp))
  `((multiple-value-setq (,(temp-entry-p-var clause)
                          ,(temp-key-var clause)
                          ,(temp-value-var clause))
      (,(iterator-var clause)))
    (unless ,(temp-entry-p-var clause)
      (go ,*epilogue-tag*))))

(defmethod step-outro-forms ((clause for-as-hash-key) initialp)
  (declare (ignore initialp))
  (nconc (destructuring-set (var clause)
                            (temp-key-var clause))
         (destructuring-set (other-var clause)
                            (temp-value-var clause))))

(defmethod step-outro-forms ((clause for-as-hash-value) initialp)
  (declare (ignore initialp))
  (nconc (destructuring-set (var clause)
                            (temp-value-var clause))
         (destructuring-set (other-var clause)
                            (temp-key-var clause))))

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

(defclass for-as-package (for-as-iteration-path form-ref-mixin)
  ((%temp-entry-p-var :accessor temp-entry-p-var)
   (%temp-symbol-var :accessor temp-symbol-var)
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITER"))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form-ref '*package*
                     :preposition-names (list '(:in :of))))


(defmethod initialize-instance :after ((instance for-as-package) &rest initargs &key)
  (declare (ignore initargs))
  (setf (temp-entry-p-var instance) (add-simple-binding instance :var "ENTRYP")
        (temp-symbol-var instance) (add-simple-binding instance :var "SYMBOL")))

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
  (setf (form-ref instance) (add-simple-binding instance :var "PKG" :form expression
                                                         :type '(or character string symbol
                                                                 package)))
  expression)

(defmethod analyze ((clause for-as-package))
  (check-nullable-simple-var-spec (var clause))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

(defmethod wrap-forms ((subclause for-as-package) forms)
  `((with-package-iterator
        (,(iterator-var subclause)
         ,(form-ref subclause)
         ,@(iterator-keywords subclause))
      ,@forms)))

(defmethod step-intro-forms ((clause for-as-package) initialp)
  (declare (ignore initialp))
  `((multiple-value-setq (,(temp-entry-p-var clause)
                          ,(temp-symbol-var clause))
      (,(iterator-var clause)))
    (unless ,(temp-entry-p-var clause)
      (go ,*epilogue-tag*))))

(defmethod step-outro-forms ((clause for-as-package) initialp)
  (declare (ignore initialp))
  (destructuring-set (var clause)
                     (temp-symbol-var clause)))

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

(defclass with-subclause-with-form (with-subclause form-ref-mixin)
  ())

;;; WITH Parsers

(defmethod parse-clause
    ((client standard-client) (scope extended-superclause) (keyword (eql :with)))
  (prog ((instance (make-instance 'with-clause
                    :start *start*))
         subclause
         subclauses
         *start*)
   next
     (setf *start* *index*
           subclause (make-instance 'with-subclause
                                    :start *start*
                                    :var (parse-d-spec :ignorable t)))
     (when (pop-token? :keywords '(:=))
       (change-class subclause 'with-subclause-with-form)
       (setf (form-ref subclause) (add-simple-binding subclause :form (pop-token))))
     (setf (end subclause) *index*)
     (push subclause subclauses)
     (when (pop-token? :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses)
           (end instance) *index*)
     (return instance)))  

(defmethod analyze ((instance with-subclause))
  (check-type-spec (var instance)))

(defmethod wrap-forms ((subclause with-subclause-with-form) forms)
  (nconc (destructuring-set (var subclause) (form-ref subclause))
         forms))
