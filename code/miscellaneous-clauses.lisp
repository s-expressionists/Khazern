(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
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

(defmethod name-clause-p ((clause name-clause))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser.

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :named)) tokens)
  (make-instance 'name-clause :name (pop-token client scope tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= return {form | it}

(defclass return-clause (unconditional-clause form-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :return)) tokens)
  (make-instance 'return-clause :form (pop-token client scope tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause return-clause))
  (let ((form (form clause)))
    `((return-from ,*loop-name*
        ,(if (and *it-var* (it-keyword-p form))
             *it-var*
             form)))))


;;;; Clause INITIAL-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INITIAL-CLAUSE.
;;;
;;; An INITIAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    initial-clause ::= initially compound-form+

(defclass initial-clause (clause compound-forms-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :initially)) tokens)
  (make-instance 'initial-clause
                 :forms (parse-compound-form+ client scope tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute prologue-form.

(defmethod prologue-forms ((clause initial-clause))
  (copy-list (forms clause)))


;;;; Clause FINAL-CLAUSE.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FINAL-CLAUSE.
;;;
;;; An FINAL clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    final-clause ::= finally compound-form+

(defclass final-clause (clause compound-forms-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser

(defmethod parse-tokens
    (client (scope body-clauses) (keyword (eql :finally)) tokens)
  (make-instance 'final-clause
                 :forms (parse-compound-form+ client scope tokens)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute epilogue.

(defmethod epilogue-forms ((clause final-clause))
  (copy-list (forms clause)))
