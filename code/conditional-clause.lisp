(cl:in-package #:khazern)

(defclass conditional-clause (selectable-clause)
  ((%condition :initarg :condition :accessor condition)
   (%then-clauses :initarg :then-clauses :reader then-clauses)
   (%else-clauses :initarg :else-clauses :reader else-clauses)))

(defmethod map-variables (function (clause conditional-clause))
  (map-variables function (then-clauses clause))
  (map-variables function (else-clauses clause)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser conditional-clause-tail ()
  (consecutive (lambda (condition then-clauses else-clauses)
                 (make-instance 'conditional-clause
                                :condition condition
                                :then-clauses then-clauses
                                :else-clauses else-clauses))
               'anything
               'selectable-clause+
               (optional nil
                         (consecutive (lambda (else-clauses)
                                        else-clauses)
                                      (keyword :else)
                                      'selectable-clause+))
               (keyword? :end)))
                         
(define-parser if-when-clauses ()
  (consecutive #'identity
               (keyword :if :when)
               'conditional-clause-tail))

(define-parser unless-clauses ()
  (consecutive (lambda (clause)
                 (setf (condition clause) `(not ,(condition clause)))
                 clause)
               (keyword :unless)
               'conditional-clause-tail))

(define-parser conditional-clause (:body-clause :selectable-clause)
  (alternative 'if-when-clauses
               'unless-clauses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-forms.

(defmethod body-forms ((clause conditional-clause) end-tag)
  (let ((*it-var* (gensym)))
    `((let ((,*it-var* ,(condition clause)))
        (cond (,*it-var*
               ,@(body-forms (car (then-clauses clause)) end-tag)
               ,@(let (*it-var*)
                   (mapcan (lambda (clause)
                             (body-forms clause end-tag))
                           (cdr (then-clauses clause)))))
              (t
               ,@(body-forms (car (else-clauses clause)) end-tag)
               ,@(let (*it-var*)
                   (mapcan (lambda (clause)
                             (body-forms clause end-tag))
                           (cdr (else-clauses clause))))))))))
