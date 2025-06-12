(cl:in-package #:khazern)

;;; Clause NAME-CLAUSE.
;;;
;;; A NAME-CLAUSE is a clause that gives a name to the loop.  It
;;; translates to a block name, so that RETURN-FROM can be used to
;;; exit the loop.  By default, the name of the loop is nil.
;;;
;;; The name-clause is optional, and if present, must be the first one
;;; in the body.  The syntax is:
;;;
;;;    NAMED name
;;;
;;; where name is a symbol.

(defclass name-clause (clause)
  ((%name :reader name
          :initarg :name)))

(defmethod clause-group ((clause name-clause))
  :name)

(defmethod (setf clause-group) (group (clause name-clause))
  (if (eq group :name)
      group
      (error 'invalid-clause-order
             :clause (subseq *body* (start clause) (end clause))
             :found-group group
             :expected-group :name)))

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :named)) tokens)
  (make-instance 'name-clause
                 :start (1- (index tokens))
                 :name (pop-token tokens)
                 :end (index tokens)))

;;; Clause RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause (unconditional-clause form-mixin)
  ())

(defmethod parse-clause
    (client (scope selectable-superclause) (keyword (eql :return)) tokens)
  (make-instance 'return-clause
                 :start (1- (index tokens))
                 :form (pop-token tokens)
                 :end (index tokens)))

(defmethod body-forms ((clause return-clause))
  `((return-from ,*loop-name*
      ,(it-form (form clause)))))

;;; Clause INITIAL-CLAUSE.
;;;
;;; An INITIAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    initial-clause ::= initially compound-form+

(defclass initial-clause (body-clause compound-forms-mixin)
  ())

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :initially)) tokens)
  (make-instance 'initial-clause
                 :start (1- (index tokens))
                 :forms (parse-compound-form+ tokens)
                 :end (index tokens)))

(defmethod prologue-forms ((clause initial-clause))
  (copy-list (forms clause)))


;;; Clause FINAL-CLAUSE.
;;;
;;; An FINAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    final-clause ::= finally compound-form+

(defclass final-clause (body-clause compound-forms-mixin)
  ())

(defmethod parse-clause
    (client (scope extended-superclause) (keyword (eql :finally)) tokens)
  (make-instance 'final-clause
                 :start (1- (index tokens))
                 :forms (parse-compound-form+ tokens)
                 :end (index tokens)))

(defmethod epilogue-forms ((clause final-clause))
  (copy-list (forms clause)))
