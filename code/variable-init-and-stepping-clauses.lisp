(cl:in-package #:khazern)

;;; 6.1.2.1 FOR-AS-CLAUSE clause
;;;
;;; The ANSI specification says that a FOR-AS-CLAUSE has the following syntax:
;;;
;;;   for-as-clause    ::= {FOR | AS} for-as-subclause {and for-as-subclause}* 
;;;   for-as-subclause ::= for-as-arithmetic | for-as-in-list | for-as-on-list |
;;;                        for-as-equals-then | for-as-across | for-as-hash | for-as-package 
;;;
;;; Since Khazern supports for-as-hash and for-as-package via for-as-being the grammar is
;;; modified:
;;;
;;;   for-as-clause    ::= {FOR | AS} for-as-subclause {and for-as-subclause}* 
;;;   for-as-subclause ::= for-as-arithmetic | for-as-in-list | for-as-on-list |
;;;                        for-as-equals-then | for-as-across | for-as-being

(defclass for-as-clause (binding-clause parallel-superclause for-as-region)
  ())

(defclass for-as-subclause (clause var-mixin)
  ())

(defun parse-for-as (client)
  (let ((instance (make-instance 'for-as-clause :start *start*)))
    (setf (subclauses instance)
          (parse-conjunctive-clauses client instance t
                                     :var (parse-d-spec :type-spec *placeholder-result*
                                                        :ignorable t))
          (end instance) *index*)
    instance))
  
(defmethod parse-clause
    ((client standard-client) (region body-region) (keyword (eql :for)) &key)
  (parse-for-as client))

(defmethod parse-clause
    ((client standard-client) (region body-region) (keyword (eql :as)) &key)
  (parse-for-as client))

(defmethod parse-clause :around
    ((client standard-client) (region for-as-region) keyword &key)
  (declare (ignore keyword))
  (parse-prepositions client (call-next-method)))

(defmethod analyze ((client standard-client) (clause for-as-subclause))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

;;; FOR-AS-BEING subclause
;;;
;;; Grammar:
;;;
;;;   for-as-being ::= BEING {EACH | THE}? name {using | preposition}*
;;;   using        ::= USING ({simple-var simple-var}+)
;;;   preposition  ::= name form

(defmethod parse-clause
    ((client standard-client) (instance for-as-clause) (keyword (eql :being)) &key var)
  (maybe-parse-token :keywords '(:each :the))
  (do-parse-clause client (make-instance 'being-region) *start* t :var var))

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

(defclass for-as-arithmetic (for-as-subclause)
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
                      :initform nil))) 

(defmethod initialize-instance :after ((instance for-as-arithmetic) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance)))

(defclass for-as-arithmetic-up (for-as-arithmetic)
  ())

(defclass for-as-arithmetic-down (for-as-arithmetic)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FOR-AS-ARITHMETIC parsers

(defmethod preposition-names ((client standard-client) (instance for-as-arithmetic))
  (values '((:from :upfrom :downfrom) (:to :upto :downto :above :below) :by)
          '()
          '()))

(defmethod parse-preposition ((client standard-client) (instance for-as-arithmetic) name)
  (let ((value (parse-token)))
    ;; parse the form
    (ecase name
      ((:to :upto :downto :above :below)
       (setf (values (end-ref instance) (end-var instance))
             (add-simple-binding instance :var :end :type 'number :form value :fold t)))
      ((:from :downfrom :upfrom)
       (setf (values (next-ref instance) (next-var instance))
             (add-simple-binding instance :var :next :type 'number :form value)))
      (:by
       (setf (values (by-ref instance) (by-var instance))
             (add-simple-binding instance :var :by :type 'number :form value :fold t))))
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
              (change-class instance 'for-as-arithmetic-down))))))
  nil)

(defun parse-for-as-arithmetic (client keyword var)
  (declare (ignore client))
  (check-nullable-simple-var-spec var)
  (unparse-token keyword)
  (make-instance 'for-as-arithmetic :start *start* :var var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :from)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :upfrom)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :downfrom)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :to)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :upto)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :downto)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :above)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :below)) &key var)
  (parse-for-as-arithmetic client keyword var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :by)) &key var)
  (parse-for-as-arithmetic client keyword var))

;;; FOR-AS-ARITHMETIC expansion methods

(defmethod analyze ((client standard-client) (clause for-as-arithmetic))
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
        (setf (values (next-ref clause) (next-var clause))
              (add-simple-binding clause :var :next :type 'number :form 0)))
      (with-accessors ((next-type type-spec)
                       (next-form form))
          next-var
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

(defclass being-cons (for-as-subclause)
  ((%by-ref :accessor by-ref
            :initform '#'cdr)
   (%rest-var :accessor rest-var)))

(defmethod initialize-instance :after ((instance being-cons) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance)))

(defmethod preposition-names ((client standard-client) (instance being-cons))
  (values '((:in :of) :by)
          '((:in :of))
          '()))

(defun parse-being-cons-of (instance)
  (setf (rest-var instance) (add-simple-binding instance :var :rest :form (parse-token))))

(defmethod parse-preposition ((client standard-client) (instance being-cons) (name (eql :in)))
  (parse-being-cons-of instance))

(defmethod parse-preposition ((client standard-client) (instance being-cons) (name (eql :of)))
  (parse-being-cons-of instance))

(defmethod parse-preposition ((client standard-client) (instance being-cons) (name (eql :by)))
  (setf (by-ref instance)
        (add-simple-binding instance :var :by :form (parse-token) :fold t
                                     :fold-test 'function-operator-p)))

(defclass being-cars (being-cons)
  ())

(defclass being-lists (being-cons)
  ())

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :in)) &key var)
  (unparse-token :of)
  (make-instance 'being-cars :start *start* :var var))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :on)) &key var)
  (unparse-token :of)
  (make-instance 'being-lists :start *start* :var var))

(defmethod step-intro-forms ((clause being-cons) initialp)
  (unless initialp
    `((setq ,(rest-var clause)
            ,(if (function-operator-p (by-ref clause))
                 `(,(second (by-ref clause)) ,(rest-var clause))
                 `(funcall ,(by-ref clause) ,(rest-var clause)))))))

(defmethod step-intro-forms :around ((clause being-cars) initialp)
  (declare (ignore initialp))
  (nconc (call-next-method)
         `((when (endp ,(rest-var clause))
             (go ,*epilogue-tag*)))))

(defmethod step-outro-forms ((clause being-cars) initialp)
  (declare (ignore initialp))
  (expand-assignments (var clause) `(car ,(rest-var clause))))

(defmethod step-intro-forms :around ((clause being-lists) initialp)
  (declare (ignore initialp))
  (nconc (call-next-method)
         `((when (atom ,(rest-var clause))
             (go ,*epilogue-tag*)))))

(defmethod step-outro-forms ((clause being-lists) initialp)
  (declare (ignore initialp))
  (expand-assignments (var clause) (rest-var clause)))

;;; 6.1.2.1.4 FOR-AS-EQUALS-THEN subclause
;;;
;;; Grammar:
;;;
;;;   for-as-equals-then := var [type-spec] = form [THEN form2]

(defclass for-as-equals-then (for-as-subclause)
  ((%temp-ref :accessor temp-ref)
   (%initial-form :accessor initial-form
                  :initarg :initial-form)
   (%subsequent-form :accessor subsequent-form
                     :initarg :subsequent-form)))

(defmethod initialize-instance :after ((instance for-as-equals-then) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance))
  (setf (temp-ref instance) (add-simple-binding instance :var :tmp)))

(defmethod preposition-names ((client standard-client) (instance for-as-equals-then))
  (values '(:= :then)
          '(:=)
          '()))

(defmethod parse-preposition
    ((client standard-client) (instance for-as-equals-then) (name (eql :=)))
  (setf (initial-form instance) (parse-token)
        (subsequent-form instance) (initial-form instance)))

(defmethod parse-preposition
    ((client standard-client) (instance for-as-equals-then) (name (eql :then)))
  (setf (subsequent-form instance) (parse-token)))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :=)) &key var)
  (unparse-token keyword)
  (make-instance 'for-as-equals-then :start *start* :var var))

(defmethod analyze ((client standard-client) (clause for-as-equals-then))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

(defmethod step-intro-forms ((clause for-as-equals-then) initialp)
  `((setq ,(temp-ref clause) ,(if initialp
                                  (initial-form clause)
                                  (subsequent-form clause)))))

(defmethod step-outro-forms ((clause for-as-equals-then) initialp)
  (declare (ignore initialp))
  (expand-assignments (var clause) (temp-ref clause)))

;;; 6.1.2.1.5 FOR-AS-ACROSS subclause
;;;
;;; Grammar:
;;;
;;;  for-as-across ::= var [type-spec] ACROSS vector

(defclass being-vector-elements (for-as-subclause form-ref-mixin)
  ((%length-ref :accessor length-ref)
   (%index-ref :accessor index-ref)))

(defmethod initialize-instance :after ((instance being-vector-elements) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance))
  (setf (length-ref instance) (add-simple-binding instance :var :length :form 0
                                                           :type 'fixnum)
        (index-ref instance) (add-simple-binding instance :var :index :form 0
                                                          :type 'fixnum)))

(defmethod preposition-names ((client standard-client) (instance being-vector-elements))
  (values '((:in :of))
          '((:in :of))
          '()))

(defun parse-being-vector-elements-of (instance)
  (setf (form-ref instance) (add-simple-binding instance :var :vector
                                                         :form (parse-token)
                                                         :type 'vector)))

(defmethod parse-preposition
    ((client standard-client) (instance being-vector-elements) (name (eql :in)))
  (parse-being-vector-elements-of instance))

(defmethod parse-preposition
    ((client standard-client) (instance being-vector-elements) (name (eql :of)))
  (parse-being-vector-elements-of instance))

(defmethod parse-clause
    ((client standard-client) (region for-as-region) (keyword (eql :across)) &key var)
  (unparse-token :of)
  (make-instance 'being-vector-elements :start *start* :var var))

(defmethod step-intro-forms ((clause being-vector-elements) initialp)
  `(,(if initialp
         `(setq ,(length-ref clause) (length ,(form-ref clause)))
         `(incf ,(index-ref clause)))
    (when (>= ,(index-ref clause) ,(length-ref clause))
      (go ,*epilogue-tag*))))

(defmethod step-outro-forms ((clause being-vector-elements) initialp)
  (declare (ignore initialp))
  (expand-assignments (var clause) `(aref ,(form-ref clause) ,(index-ref clause))))

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

(defclass being-hash-entries (for-as-subclause form-ref-mixin other-var-mixin)
  ((%temp-entry-p-var :accessor temp-entry-p-var)
   (%temp-key-var :accessor temp-key-var)
   (%temp-value-var :accessor temp-value-var)
   (%iterator-var :reader iterator-var
                  :initform (unique-name :iter)))
  (:default-initargs :other-var (make-instance 'destructuring-binding
                                               :var-spec nil)))

(defmethod initialize-instance :after ((instance being-hash-entries) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance))
  (setf (temp-entry-p-var instance) (add-simple-binding instance :var :entryp)
        (temp-key-var instance) (add-simple-binding instance :var :key :ignorable t)
        (temp-value-var instance) (add-simple-binding instance :var :value :ignorable t)))

(defclass being-hash-keys (being-hash-entries)
  ())

(defclass being-hash-values (being-hash-entries)
  ())

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :hash-key)) &key var)
  (make-instance 'being-hash-keys :var var :start *start*))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :hash-keys)) &key var)
  (make-instance 'being-hash-keys :var var :start *start*))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :hash-value)) &key var)
  (make-instance 'being-hash-values :var var :start *start*))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :hash-values)) &key var)
  (make-instance 'being-hash-values :var var :start *start*))

(defmethod preposition-names ((client standard-client) (instance being-hash-keys))
  (values '((:in :of))
          '((:in :of))
          '(:hash-value)))
                     
(defmethod preposition-names ((client standard-client) (instance being-hash-values))
  (values '((:in :of))
          '((:in :of))
          '(:hash-key)))

(defmethod parse-preposition ((client standard-client) (instance being-hash-entries) key)
  (declare (ignore key))
  (when (var-spec (other-var instance))
    (warn 'preposition-order
          :first-preposition :using
          :second-preposition :in
          :name (if (typep instance 'being-hash-keys)
                    :hash-key
                    :hash-value)
          :clause (when (numberp *start*)
                    (subseq *body* *start* *index*))))
  (setf (form-ref instance) (add-simple-binding instance :var :ht :form (parse-token)
                                                         :type 'hash-table)))

(defun parse-being-hash-entries-other (instance)
  (setf (other-var instance) (add-binding instance
                                          (parse-var-spec :ignorable t))))

(defmethod parse-using
    ((client standard-client) (instance being-hash-keys) (key (eql :hash-value)))
  (parse-being-hash-entries-other instance))

(defmethod parse-using
    ((client standard-client) (instance being-hash-values) (key (eql :hash-key)))
  (parse-being-hash-entries-other instance))

(defmethod analyze ((client standard-client) (clause being-hash-entries))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

(defmethod wrap-forms ((subclause being-hash-entries) forms)
  `((with-hash-table-iterator
        (,(iterator-var subclause) ,(form-ref subclause))
      ,@forms)))

(defmethod step-intro-forms ((clause being-hash-entries) initialp)
  (declare (ignore initialp))
  `((multiple-value-setq (,(temp-entry-p-var clause)
                          ,(temp-key-var clause)
                          ,(temp-value-var clause))
      (,(iterator-var clause)))
    (unless ,(temp-entry-p-var clause)
      (go ,*epilogue-tag*))))

(defmethod step-outro-forms ((clause being-hash-keys) initialp)
  (declare (ignore initialp))
  (expand-assignments (var clause) (temp-key-var clause)
                      (other-var clause) (temp-value-var clause)))

(defmethod step-outro-forms ((clause being-hash-values) initialp)
  (declare (ignore initialp))
  (expand-assignments (var clause) (temp-value-var clause)
                      (other-var clause) (temp-key-var clause)))

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

(defclass being-package-symbols (for-as-subclause form-ref-mixin)
  ((%entryp-var :accessor entryp-var)
   (%sym-var :accessor sym-var)g
   (%acc-type-var :accessor acc-type-var)
   (%pkg-var :accessor pkg-var)
   (%acc-type-ref :accessor acc-type-ref
                  :initform nil)
   (%pkg-ref :accessor pkg-ref
             :initform nil)
   (%iterator-var :reader iterator-var
                  :initform (unique-name :iter))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form-ref '*package*))

(defmethod initialize-instance :after ((instance being-package-symbols) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance))
  (setf (entryp-var instance) (add-simple-binding instance :var :entryp)
        (sym-var instance) (add-simple-binding instance :var :sym
                                                        :type 'symbol
                                                        :ignorable t)
        (acc-type-var instance) (add-simple-binding instance :var :acc
                                                             :type 'symbol
                                                             :ignorable t)
        (pkg-var instance) (add-simple-binding instance :var :pkg
                                                        :type '(or character string symbol
                                                                   package)
                                                        :ignorable t)))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :symbol)) &key var)
  (make-instance 'being-package-symbols
                 :start *start*
                 :var var
                 :iterator-keywords '(:internal :external :inherited)))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :symbols)) &key var)
  (make-instance 'being-package-symbols
                 :start *start*
                 :var var
                 :iterator-keywords '(:internal :external :inherited)))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :present-symbol))
     &key var)
  (make-instance 'being-package-symbols
                 :start *start*
                 :var var
                 :iterator-keywords '(:internal :external)))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :present-symbols))
     &key var)
  (make-instance 'being-package-symbols
                 :start *start*
                 :var var
                 :iterator-keywords '(:internal :external)))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :external-symbol))
     &key var)
  (make-instance 'being-package-symbols
                 :start *start*
                 :var var
                 :iterator-keywords '(:external)))

(defmethod parse-clause
    ((client standard-client) (region being-region) (name (eql :external-symbols))
     &key var)
  (make-instance 'being-package-symbols
                 :start *start*
                 :var var
                 :iterator-keywords '(:external)))

(defmethod preposition-names ((client standard-client) (instance being-package-symbols))
  (values '((:in :of))
          '()
          '()))

(defun parse-being-package-symbols-of (instance)
  (setf (form-ref instance) (add-simple-binding instance :var :pkg :form (parse-token)
                                                         :type '(or character string symbol
                                                                    package list))))

(defmethod parse-preposition
    ((client standard-client) (instance being-package-symbols) (key (eql :in)))
  (parse-being-package-symbols-of instance))

(defmethod parse-preposition
    ((client standard-client) (instance being-package-symbols) (key (eql :of)))
  (parse-being-package-symbols-of instance))

(defmethod parse-using
    ((client standard-client) (instance being-package-symbols) (key (eql :accessibility-type)))
  (setf (acc-type-ref instance) (add-simple-binding instance :var (parse-token :type 'simple-var)
                                                             :form nil
                                                             :type 'symbol)))

(defmethod parse-using
    ((client standard-client) (instance being-package-symbols) (key (eql :package)))
  (setf (pkg-ref instance) (add-simple-binding instance :var (parse-token :type 'simple-var)
                                                        :form nil
                                                        :type '(or character string symbol
                                                                   package))))

(defmethod analyze ((client standard-client) (clause being-package-symbols))
  (check-nullable-simple-var-spec (var clause))
  (when (eq (type-spec (var clause)) *placeholder-result*)
    (setf (type-spec (var clause)) t))
  (check-type-spec (var clause)))

(defmethod wrap-forms ((subclause being-package-symbols) forms)
  `((with-package-iterator
        (,(iterator-var subclause)
         ,(form-ref subclause)
         ,@(iterator-keywords subclause))
      ,@forms)))

(defmethod step-intro-forms ((clause being-package-symbols) initialp)
  (declare (ignore initialp))
  (with-accessors ((entryp-var entryp-var)
                   (sym-var sym-var)
                   (acc-type-var acc-type-var)
                   (pkg-var pkg-var)
                   (iterator-var iterator-var))
      clause
    `((multiple-value-setq (,entryp-var ,sym-var ,acc-type-var ,pkg-var)
        (,iterator-var))
      (unless ,entryp-var
        (go ,*epilogue-tag*)))))

(defmethod step-outro-forms ((clause being-package-symbols) initialp)
  (declare (ignore initialp))
  (with-accessors ((sym-var sym-var)
                   (acc-type-var acc-type-var)
                   (acc-type-ref acc-type-ref)
                   (pkg-var pkg-var)
                   (pkg-ref pkg-ref)
                   (var var))
      clause
    (expand-assignments var sym-var
                        acc-type-ref acc-type-var
                        pkg-ref pkg-var)))

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

(defclass with-clause (binding-clause parallel-superclause with-region)
  ())

(defclass with-subclause (clause var-mixin)
  ())

(defclass with-subclause-with-form (with-subclause form-ref-mixin)
  ())

(defmethod preposition-names ((client standard-client) (instance with-subclause))
  (values '(:=)
          '()
          '()))

(defmethod parse-preposition ((client standard-client) (instance with-subclause) (name (eql :=)))
  (change-class instance 'with-subclause-with-form)
  (setf (form-ref instance) (add-simple-binding instance :form (parse-token))))

(defmethod initialize-instance :after ((instance with-subclause) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance)))

;;; WITH Parsers

(defmethod parse-clause
    ((client standard-client) (region body-region) (keyword (eql :with)) &key)
  (let ((instance (make-instance 'with-clause :start *start*)))
    (setf (subclauses instance)
          (parse-conjunctive-clauses client instance nil
                                     :var (parse-d-spec :ignorable t))
          (end instance) *index*)
    instance))

(defmethod parse-clause :around
    ((client standard-client) (region with-region) keyword &key)
  (declare (ignore keyword))
  (parse-prepositions client (call-next-method)))

(defmethod parse-clause ((client standard-client) (region with-region) (keyword null) &key var)
  (make-instance 'with-subclause
                 :start *start*
                 :var var))

(defmethod analyze ((client standard-client) (instance with-subclause))
  (check-type-spec (var instance)))

(defmethod wrap-forms ((subclause with-subclause-with-form) forms)
  (nconc (expand-assignments (var subclause) (form-ref subclause))
         forms))
