(cl:in-package #:khazern)

(defclass conditional-clause (body-clause selectable-region)
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

(defmethod analyze ((client standard-client) (clause conditional-clause))
  (mapc (lambda (clause)
          (analyze client clause))
        (then-clauses clause))
  (mapc (lambda (clause)
          (analyze client clause))
        (else-clauses clause)))

(defun parse-conditional-clause-tail (client instance)
  (setf (then-clauses instance)
        (parse-conjunctive-clauses client instance))
  (when (maybe-parse-token :keywords '(:else))
    (setf (else-clauses instance)
          (parse-conjunctive-clauses client instance)))
  (maybe-parse-token :keywords '(:end))
  (setf (end instance) *index*)
  instance)

(defmethod parse-clause (client (region selectable-region) (keyword (eql :if)) &key)
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :start *start*
                                                :condition (parse-token))))

(defmethod parse-clause (client (region selectable-region) (keyword (eql :when)) &key)
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :start *start*
                                                :condition (parse-token))))

(defmethod parse-clause (client (region selectable-region) (keyword (eql :unless)) &key)
  (parse-conditional-clause-tail client
                                 (make-instance 'conditional-clause
                                                :start *start*
                                                :condition `(not ,(parse-token)))))

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
