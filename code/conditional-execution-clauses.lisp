(cl:in-package #:khazern)

(defclass conditional-clause (selectable-clauses)
  ((%condition :accessor condition
               :initarg :condition)
   (%then-clauses :accessor then-clauses
                  :initarg :then-clauses)
   (%else-clauses :accessor else-clauses
                  :initarg :else-clauses
                  :initform nil)))

(defmethod map-variables (function (clause conditional-clause))
  (map-variables function (then-clauses clause))
  (map-variables function (else-clauses clause)))

(defmethod analyze ((clause conditional-clause))
  (mapc #'analyze (then-clauses clause))
  (mapc #'analyze (else-clauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(defun parse-conditional-clause-tail (client instance tokens)
  (setf (then-clauses instance)
        (parse-parallel-clauses client instance tokens))
  (when (pop-token? tokens :keywords '(:else))
    (setf (else-clauses instance)
          (parse-parallel-clauses client instance tokens)))
  (pop-token? tokens :keywords '(:end))
  instance)

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :if)) tokens)
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :condition (pop-token tokens))
                                 tokens))

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :when)) tokens)
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :condition (pop-token tokens))
                                 tokens))

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :unless)) tokens)
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :condition `(not ,(pop-token tokens)))
                                 tokens))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause conditional-clause))
  (let ((*it-var* (gensym)))
    `((let ((,*it-var* ,(condition clause)))
        (cond (,*it-var*
               ,@(body-forms (car (then-clauses clause)))
               ,@(let (*it-var*)
                   (mapcan #'body-forms (cdr (then-clauses clause)))))
              (t
               ,@(body-forms (car (else-clauses clause)))
               ,@(let (*it-var*)
                   (mapcan #'body-forms (cdr (else-clauses clause))))))))))
