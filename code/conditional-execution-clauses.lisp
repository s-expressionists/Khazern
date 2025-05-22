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
  (when (pop-token? client instance tokens '(eql :else))
    (setf (else-clauses instance)
          (parse-parallel-clauses client instance tokens)))
  (pop-token? client instance tokens '(eql :end))
  instance)

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :if)) tokens)
  (parse-conditional-clause-tail client
                               (make-instance 'conditional-clause
                                              :condition (pop-token client scope tokens))
                               tokens))

(defmethod parse-tokens
    (client (scope selectable-clauses) (keyword (eql :unless)) tokens)
  (parse-conditional-clause-tail client
                               (make-instance 'conditional-clause
                                              :condition `(not ,(pop-token client scope tokens)))
                               tokens))

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
