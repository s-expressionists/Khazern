(cl:in-package #:khazern)

(defclass conditional-clause (selectable-clause)
  ((%condition :initarg :condition :reader condition)
   (%then-clauses :initarg :then-clauses :reader then-clauses)
   (%else-clauses :initarg :else-clauses :reader else-clauses)))

;;; A conditional clause does not introduce any bindings for any
;;; variables, so this method should return the empty list.
(defmethod bound-variables ((clause conditional-clause))
  '())

(defmethod accumulation-variables ((clause conditional-clause))
  (append (reduce #'append
                  (mapcar #'accumulation-variables (then-clauses clause)))
          (reduce #'append
                  (mapcar #'accumulation-variables (else-clauses clause)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parsers.

(define-parser if-else-end-clause ()
  (consecutive (lambda (if form then-clauses else else-clauses end)
                 (declare (ignore if else end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword 'if 'when)
               'anything
               'selectable-clause+
               (keyword 'else)
               'selectable-clause+
               (keyword 'end)))

(define-parser if-end-clause ()
  (consecutive (lambda (if form then-clauses end)
                 (declare (ignore if end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses nil))
               (keyword 'if 'when)
               'anything
               'selectable-clause+
               (keyword 'end)))
               
(define-parser if-else-clause ()
  (consecutive (lambda (if form then-clauses else else-clauses)
                 (declare (ignore if else))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword 'if 'when)
               'anything
               'selectable-clause+
               (keyword 'else)
               'selectable-clause+))

(define-parser if-clause ()
  (consecutive (lambda (if form then-clauses)
                 (declare (ignore if))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses nil))
               (keyword 'if 'when)
               'anything
               'selectable-clause+))

(define-parser if-when-clauses ()
  (alternative 'if-else-end-clause
               'if-end-clause
               'if-else-clause
               'if-clause))

(define-parser unless-else-end-clause ()
  (consecutive (lambda (unless form else-clauses else then-clauses end)
                 (declare (ignore unless else end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword 'unless)
               'anything
               'selectable-clause+
               (keyword 'else)
               'selectable-clause+
               (keyword 'end)))

(define-parser unless-end-clause ()
  (consecutive (lambda (unless form else-clauses end)
                 (declare (ignore unless end))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses nil
                   :else-clauses else-clauses))
               (keyword 'unless)
               'anything
               'selectable-clause+
               (keyword 'end)))
               
(define-parser unless-else-clause ()
  (consecutive (lambda (unless form else-clauses else then-clauses)
                 (declare (ignore unless else))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses then-clauses
                   :else-clauses else-clauses))
               (keyword 'unless)
               'anything
               'selectable-clause+
               (keyword 'else)
               'selectable-clause+))

(define-parser unless-clause ()
  (consecutive (lambda (unless form else-clauses)
                 (declare (ignore unless))
                 (make-instance 'conditional-clause
                   :condition form
                   :then-clauses nil
                   :else-clauses else-clauses))
               (keyword 'unless 'when)
               'anything
               'selectable-clause+))

(define-parser unless-clauses ()
  (alternative 'unless-else-end-clause
               'unless-end-clause
               'unless-else-clause
               'unless-clause))

(define-parser conditional-clause (:body-clause :selectable-clause)
  (alternative 'if-when-clauses
               'unless-clauses))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compute body-form.

(defmethod body-form ((clause conditional-clause) end-tag)
  (let ((*it-var* (gensym)))
    `(let ((,*it-var* ,(condition clause)))
       (if ,*it-var*
           (progn
             ,(body-form (car (then-clauses clause)) end-tag)
             ,@(let (*it-var*)
                 (mapcar (lambda (clause)
                           (body-form clause end-tag))
                         (cdr (then-clauses clause)))))
           (progn
             ,(body-form (car (else-clauses clause)) end-tag)
             ,@(let (*it-var*)
                 (mapcar (lambda (clause)
                           (body-form clause end-tag))
                         (cdr (else-clauses clause)))))))))
