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

(defclass for-as-clause (binding-clause parallel-superclause for-as-region)
  ())

(defclass for-as-subclause (clause var-mixin)
  ())

(defun parse-for-as (client)
  (let ((instance (make-instance 'for-as-clause :start *start*)))
    (setf (subclauses instance)
          (parse-conjunctive-clauses client instance
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

(defmethod analyze ((client standard-client) (clause for-as-subclause))
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

(defmethod make-iteration-path (client name var &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore var inclusive-form))
  (error 'unknown-iteration-path
         :client client
         :name name
         :inclusive inclusive-form-p
         :clause (when (numberp *start*)
                   (subseq *body* *start* *index*))))


(defmethod parse-clause
    ((client standard-client) (instance for-as-clause) (keyword (eql :being)) &key var)
  (let ((instance (if (maybe-parse-token :keywords '(:each :the))
                      (make-iteration-path client
                                           (make-iteration-path-name client
                                                                     (parse-token))
                                           var)
                      (let ((form (parse-token)))
                        (parse-token :keywords '(:and))
                        (parse-token :keywords '(:its :each :his :her))
                        (make-iteration-path client
                                             (make-iteration-path-name client
                                                                       (parse-token) t)
                                             var
                                             form)))))
    (parse-prepositions client instance)
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
  (values '((:from :upfrom :downfrom)
            (:to :upto :downto :above :below)
            :by)
          '()
          '()))

(defmethod parse-preposition ((client standard-client) (instance for-as-arithmetic) name)
  (let ((value (parse-token)))
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
              (change-class instance 'for-as-arithmetic-down))))))
  nil)

(defun parse-for-as-arithmetic (client keyword var)
  (check-nullable-simple-var-spec var)
  (let ((instance (make-instance 'for-as-arithmetic
                                 :start *start*
                                 :var var)))
    (unparse-token keyword)
    (parse-prepositions client instance)
    (setf (end instance) *index*)
    instance))

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
              (add-simple-binding clause :var "NEXT" :type 'number :form 0)))
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

(defclass for-as-list (for-as-subclause)
  ((%by-ref :accessor by-ref
            :initform '#'cdr)
   (%rest-var :accessor rest-var)))

(defmethod initialize-instance :after ((instance for-as-list) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance)))


(defclass for-as-in-list (for-as-list)
  ())

(defclass for-as-on-list (for-as-list)
  ())

(defun parse-for-as-list (instance)
  (setf (rest-var instance) (add-simple-binding instance :var "REST" :form (parse-token)))
  (when (maybe-parse-token :keywords '(:by))
    (setf (by-ref instance) (add-simple-binding instance :var "BY" :form (parse-token) :fold t
                                                         :fold-test 'function-operator-p)))
  (setf (end instance) *index*)
  instance)

(defmethod parse-clause ((client standard-client) (region for-as-region) (keyword (eql :in)) &key var)
  (parse-for-as-list (make-instance 'for-as-in-list :start *start* :var var)))

(defmethod parse-clause ((client standard-client) (region for-as-region) (keyword (eql :on)) &key var)
  (parse-for-as-list (make-instance 'for-as-on-list :start *start* :var var)))

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
  (add-binding instance (var instance))
  (setf (temp-ref instance) (add-simple-binding instance :var "TMP")))

(defmethod parse-clause ((client standard-client) (region for-as-region) (keyword (eql :=)) &key var)
  (let ((initial-form (parse-token)))
    (make-instance 'for-as-equals-then
                   :start *start*
                   :var var
                   :initial-form initial-form
                   :subsequent-form (if (maybe-parse-token :keywords '(:then))
                                        (parse-token)
                                        initial-form)
                   :end *index*)))

(defmethod analyze ((client standard-client) (clause for-as-equals-then))
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
  (add-binding instance (var instance))
  (setf (length-ref instance) (add-simple-binding instance :var "LENGTH" :form 0
                                                           :type 'fixnum)
        (index-ref instance) (add-simple-binding instance :var "INDEX" :form 0
                                                          :type 'fixnum)))

(defmethod parse-clause ((client standard-client) (region for-as-region) (keyword (eql :across)) &key var)
  (let ((instance (make-instance 'for-as-across
                                 :start *start*
                                 :var var)))
    (setf (form-ref instance) (add-simple-binding instance :var "VECTOR" :form (parse-token)
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

(defclass for-as-hash (for-as-subclause form-ref-mixin other-var-mixin)
  ((%temp-entry-p-var :accessor temp-entry-p-var)
   (%temp-key-var :accessor temp-key-var)
   (%temp-value-var :accessor temp-value-var)
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITER")))
  (:default-initargs :other-var (make-instance 'destructuring-binding
                                               :var-spec nil)))

(defmethod initialize-instance :after ((instance for-as-hash) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance))
  (setf (temp-entry-p-var instance) (add-simple-binding instance :var "ENTRYP")
        (temp-key-var instance) (add-simple-binding instance :var "KEY")
        (temp-value-var instance) (add-simple-binding instance :var "VALUE")))

(defclass for-as-hash-key (for-as-hash)
  ())

(defclass for-as-hash-value (for-as-hash)
  ())

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-key)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-key
                     :var var
                     :start *start*)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-keys)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-key
                     :var var
                     :start *start*)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-value)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-value
                     :var var
                     :start *start*)))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :hash-values)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-hash-value
                     :var var
                     :start *start*)))

(defmethod preposition-names ((client standard-client) (instance for-as-hash-key))
  (values '((:in :of))
          '((:in :of))
          '(:hash-value)))
                     
(defmethod preposition-names ((client standard-client) (instance for-as-hash-value))
  (values '((:in :of))
          '((:in :of))
          '(:hash-key)))

(defmethod parse-preposition ((client standard-client) (instance for-as-hash) key)
  (when (var-spec (other-var instance))
    (warn 'invalid-iteration-path-preposition-order
          :first-preposition :using
          :second-preposition :in
          :name (if (typep instance 'for-as-hash-key)
                    :hash-key
                    :hash-value)
          :clause (when (numberp *start*)
                    (subseq *body* *start* *index*))))
  (setf (form-ref instance) (add-simple-binding instance :var "HT" :form (parse-token)
                                                         :type 'hash-table)))

(defmethod parse-using ((client standard-client) (instance for-as-hash) key)
  (declare (ignore key))
  (setf (other-var instance) (add-binding instance
                                          (parse-var-spec :ignorable t))))

(defmethod analyze ((client standard-client) (clause for-as-hash))
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

(defclass for-as-package (for-as-subclause form-ref-mixin)
  ((%temp-entry-p-var :accessor temp-entry-p-var)
   (%temp-symbol-var :accessor temp-symbol-var)
   (%iterator-var :reader iterator-var
                  :initform (gensym "ITER"))
   (%iterator-keywords :reader iterator-keywords
                       :initarg :iterator-keywords))
  (:default-initargs :form-ref '*package*))

(defmethod initialize-instance :after ((instance for-as-package) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance))
  (setf (temp-entry-p-var instance) (add-simple-binding instance :var "ENTRYP")
        (temp-symbol-var instance) (add-simple-binding instance :var "SYMBOL")))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :symbol)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :var var
                     :iterator-keywords '(:internal :external :inherited))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :symbols)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :var var
                     :iterator-keywords '(:internal :external :inherited))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :present-symbol)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :var var
                     :iterator-keywords '(:internal :external))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :present-symbols)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :var var
                     :iterator-keywords '(:internal :external))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :external-symbol)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :var var
                     :iterator-keywords '(:external))))

(defmethod make-iteration-path
    ((client standard-client) (name (eql :external-symbols)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-package
                     :start *start*
                     :var var
                     :iterator-keywords '(:external))))

(defmethod preposition-names ((client standard-client) (instance for-as-package))
  (values '((:in :of))
          '()
          '()))

(defmethod parse-preposition ((client standard-client) (instance for-as-package) key)
  (setf (form-ref instance) (add-simple-binding instance :var "PKG" :form (parse-token)
                                                         :type '(or character string symbol
                                                                 package))))

(defmethod analyze ((client standard-client) (clause for-as-package))
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

(defmethod initialize-instance :after ((instance with-subclause) &rest initargs &key)
  (declare (ignore initargs))
  (add-binding instance (var instance)))

;;; WITH Parsers

(defmethod parse-clause
    ((client standard-client) (region body-region) (keyword (eql :with)) &key)
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
     (when (maybe-parse-token :keywords '(:=))
       (change-class subclause 'with-subclause-with-form)
       (setf (form-ref subclause) (add-simple-binding subclause :form (parse-token))))
     (setf (end subclause) *index*)
     (push subclause subclauses)
     (when (maybe-parse-token :keywords '(:and))
       (go next))
     (setf (subclauses instance) (nreverse subclauses)
           (end instance) *index*)
     (return instance)))  

(defmethod analyze ((client standard-client) (instance with-subclause))
  (check-type-spec (var instance)))

(defmethod wrap-forms ((subclause with-subclause-with-form) forms)
  (nconc (destructuring-set (var subclause) (form-ref subclause))
         forms))
