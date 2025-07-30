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
    ((client standard-client) (region body-region) (keyword (eql :named)) &key)
  (make-instance 'name-clause
                 :start *start*
                 :name (parse-token)
                 :end *index*))

;;; Clause RETURN-CLAUSE.
;;;
;;; An RETURN clause does not exist as a separate grammar item in
;;; the HyperSpec, but we define it here anyway.  The syntax is:
;;;
;;;    return-clause ::= RETURN {form | IT}

(defclass return-clause (unconditional-clause form-mixin)
  ())

(defmethod parse-clause
    ((client standard-client) (region selectable-region) (keyword (eql :return)) &key)
  (make-instance 'return-clause
                 :start *start*
                 :form (parse-token)
                 :end *index*))

(defmethod body-forms ((clause return-clause))
  `((return-from ,*loop-name*
      ,(it-form (form clause)))))

;;; Clause INITIAL-CLAUSE.
;;;
;;; An INITIAL clause does not exist as a separate grammar item in the HyperSpec, but we define
;;; it here anyway.  The syntax is:
;;;
;;;    initial-clause ::= INITIALLY compound-form+

(defclass initial-clause (body-clause compound-forms-mixin)
  ())

(defmethod parse-clause
    ((client standard-client) (region body-region) (keyword (eql :initially)) &key)
  (make-instance 'initial-clause
                 :start *start*
                 :forms (parse-compound-forms)
                 :end *index*))

(defmethod prologue-forms ((clause initial-clause))
  (copy-list (forms clause)))


;;; Clause FINAL-CLAUSE.
;;;
;;; An FINAL clause does not exist as a separate grammar item in the HyperSpec, but we define it
;;; here anyway.  The syntax is:
;;;
;;;    final-clause ::= FINALLY compound-form+

(defclass final-clause (body-clause compound-forms-mixin)
  ())

(defmethod parse-clause
    ((client standard-client) (region body-region) (keyword (eql :finally)) &key)
  (make-instance 'final-clause
                 :start *start*
                 :forms (parse-compound-forms)
                 :end *index*))

(defmethod epilogue-forms ((clause final-clause))
  (copy-list (forms clause)))
