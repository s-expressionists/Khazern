(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Syntactic and semantic analysis

;;; Check that if there is a name-clause, the last one is in position
;;; zero.
(defun check-name-clause-position (clauses)
  (let ((name-clause-position
          (position-if #'name-clause-p clauses :from-end t)))
    (when (and (not (null name-clause-position)) (plusp name-clause-position))
      (error 'name-clause-not-first))))

;;; Check that there is not a variable-clause following a main clause.
;;; Recall that we diverge from the BNF grammar in the HyperSpec so
;;; that INITIALLY and FINALLY are neither main clauses nor variable
;;; clauses.
(defun check-order-variable-clause-main-clause (clauses)
  (let ((last-variable-clause-position
          (position-if #'variable-clause-p clauses :from-end t))
        (first-main-clause-position
          (position-if #'main-clause-p clauses)))
    (when (and (not (null last-variable-clause-position))
               (not (null first-main-clause-position))
               (> last-variable-clause-position first-main-clause-position))
      (error 'invalid-clause-order))))

(defun verify-clause-order (clauses)
  (check-name-clause-position clauses)
  (check-order-variable-clause-main-clause clauses))

(defun check-variables (clauses)
  (let ((variables nil))
    (map-variables (lambda (name type category)
                     (declare (ignore type))
                     (let ((current-category (getf variables name)))
                       (cond ((null current-category)
                              (setf (getf variables name) category))
                             ((and (eq category t)
                                   (eq current-category t))
                              (error 'multiple-variable-occurrences
                                     :bound-variable name))
                             ((or (eq category t)
                                  (eq current-category t))
                              (error 'iteration-accumulation-overlap
                                     :bound-variable name))
                             ((not (eq category current-category))
                              (error 'multiple-accumulation-occurrences
                                     :bound-variable name
                                     :first-clause category
                                     :second-clause current-category)))))
                   clauses)))

;;; FIXME: Add more analyses.
(defmethod analyze ((clauses cons))
  (mapc #'analyze clauses)
  (verify-clause-order clauses)
  (check-variables clauses))
