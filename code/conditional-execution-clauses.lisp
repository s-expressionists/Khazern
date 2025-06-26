(cl:in-package #:khazern)

(defclass conditional-clause (selectable-superclause)
  ((%condition :accessor condition
               :initarg :condition)
   (%then-clauses :accessor then-clauses
                  :initarg :then-clauses)
   (%else-clauses :accessor else-clauses
                  :initarg :else-clauses
                  :initform nil)))

(defmethod map-variables progn (function (clause conditional-clause))
  (map-variables function (then-clauses clause))
  (map-variables function (else-clauses clause)))

(defmethod analyze ((clause conditional-clause))
  (mapc #'analyze (then-clauses clause))
  (mapc #'analyze (else-clauses clause)))

(defun parse-conditional-clause-tail (client instance)
  (setf (then-clauses instance)
        (parse-parallel-clauses client instance))
  (when (pop-token? :keywords '(:else))
    (setf (else-clauses instance)
          (parse-parallel-clauses client instance)))
  (pop-token? :keywords '(:end))
  (setf (end instance) *index*)
  instance)

(defmethod parse-clause (client (scope selectable-superclause) (keyword (eql :if)))
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :start *start*
                                                :condition (pop-token))))

(defmethod parse-clause (client (scope selectable-superclause) (keyword (eql :when)))
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :start *start*
                                                :condition (pop-token))))

(defmethod parse-clause (client (scope selectable-superclause) (keyword (eql :unless)))
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :start *start*
                                                :condition `(not ,(pop-token)))))

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
