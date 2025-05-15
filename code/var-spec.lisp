(cl:in-package #:khazern)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parser for d-var-spec.

;;; A d-var-spec is a is a destructuring variable specifier: 
;;; 
;;;    d-var-spec ::= simple-var | nil | (d-var-spec . d-var-spec)
;;;
;;; where simple-var is a symbol (a name of a variable). 
;;;

;;; Return true if and only if the argument is a valid d-var-spec, in
;;; other words if it is a tree of CONS cells where the leaves are
;;; symbols.
(defun d-var-spec-p (object)
  (or (null object)
      (and (symbolp object)
           (not (constantp object)))
      (and (consp object)
           (d-var-spec-p (car object))
           (d-var-spec-p (cdr object)))))

(defclass d-spec ()
  ((%var-spec :accessor var-spec
              :initarg :var-spec)
   (%type-spec :accessor type-spec
               :initarg :type-spec
               :initform t)
   (%temps :accessor temps)))

(defun set-d-spec-temps (d-spec &optional temp-var-p)
  (let ((temps (make-hash-table)))
    (labels ((traverse (d-var-spec)
               (cond ((null d-var-spec)
                      nil)
                     ((symbolp d-var-spec)
                      (when temp-var-p
                        (setf (gethash d-var-spec temps) (gensym "TMP")))
                      t)
                     ((not (consp d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     (t
                      (let ((car-p (traverse (car d-var-spec)))
                            (cdr-p (traverse (cdr d-var-spec))))
                        (when (and car-p cdr-p)
                          (setf (gethash d-var-spec temps) (gensym "DE")))
                          (or car-p cdr-p))))))
      (traverse (var-spec d-spec))
      (setf (temps d-spec) temps))))

(defmethod initialize-instance :after
    ((clause d-spec) &rest initargs &key ((:temp-var temp-var-p) nil))
  (declare (ignore initargs))
  (set-d-spec-temps clause temp-var-p))

(defun d-spec-inner-bindings (d-spec form &optional bind-all-p)
  (let ((bindings '())
        (temps (temps d-spec)))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec))
                     ((symbolp d-var-spec)
                      (let ((temp (gethash d-var-spec temps)))
                        (when temp
                          (push `(,temp ,form) bindings))
                        (when bind-all-p
                          (push `(,d-var-spec ,(or temp form)) bindings))))
                     ((consp d-var-spec)
                      (let ((temp (gethash d-var-spec temps)))
                        (cond (temp
                               (push `(,temp ,form) bindings)
                               (traverse (car d-var-spec) `(car ,temp))
                               (traverse (cdr d-var-spec) `(cdr ,temp)))
                              (t
                               (traverse (car d-var-spec) `(car ,form))
                               (traverse (cdr d-var-spec) `(cdr ,form)))))))))
      (traverse (var-spec d-spec) form)
      (nreverse bindings))))

(defun d-spec-inner-assignments (d-spec form)
  (let ((assignments '())
        (temps (temps d-spec)))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec))
                     ((symbolp d-var-spec)
                      (push d-var-spec assignments)
                      (push (or (gethash d-var-spec temps) form) assignments))
                     ((not (consp d-var-spec))
                      (error 'expected-var-spec-but-found
                             :found d-var-spec))
                     (t
                      (let ((temp (gethash d-var-spec temps)))
                        (cond (temp
                               (traverse (car d-var-spec) `(car ,temp))
                               (traverse (cdr d-var-spec) `(cdr ,temp)))
                              (t
                               (traverse (car d-var-spec) `(car ,form))
                               (traverse (cdr d-var-spec) `(cdr ,form)))))))))
      (traverse (var-spec d-spec) form)
      (nreverse assignments))))

(defun d-spec-inner-form (d-spec form)
  (wrap-let* (d-spec-inner-bindings d-spec form)
             nil
             `((setq ,@(d-spec-inner-assignments d-spec form)))))

(defmethod map-variables (function (d-spec d-spec))
  (labels ((traverse (var-spec type-spec)
             (cond ((null var-spec))
                   ((symbolp var-spec)
                    (funcall function var-spec (or type-spec t) t))
                   ((not (consp var-spec))
                    (error 'expected-var-spec-but-found
                           :found var-spec))
                   ((symbolp type-spec)
                    (traverse (car var-spec) type-spec)
                    (traverse (cdr var-spec) type-spec))
                   ((not (consp type-spec))
                    (error 'expected-type-spec-but-found
                           :found type-spec))
                   (t
                    (traverse (car var-spec) (car type-spec))
                    (traverse (cdr var-spec) (cdr type-spec))))
             nil))
    (traverse (var-spec d-spec) (type-spec d-spec))))

(defun d-spec-outer-declarations (d-spec)
  (let ((result '())
        (ignorables '()))
    (map-variables (lambda (var type category)
                     (declare (ignore category))
                     (unless (eq type t)
                       (push `(cl:type ,(type-or-null type) ,var)
                             result))
                     (push var ignorables))
                   d-spec)
     (cons `(ignorable ,.(nreverse ignorables))
           (nreverse result))))

(defun d-spec-outer-bindings (d-spec)
  (let ((result '()))
    (map-variables (lambda (var type category)
                     (declare (ignore type category))
                     (push `(,var nil) result))
                   d-spec)
    (nreverse result)))

(defun d-spec-simple-declarations (d-spec
                                   &key ((:ignorable ignorablep) nil)
                                        ((:nullable nullablep) nil))
  (with-accessors ((var-spec var-spec)
                   (type-spec type-spec))
      d-spec
    (let (decl)
      (when var-spec
        (unless (eq type-spec t)
          (push `(cl:type ,(if nullablep
                             (type-or-null type-spec)
                             type-spec)
                          ,var-spec)
                decl))
        (when ignorablep
          (push `(ignorable ,var-spec) decl)))
      decl)))

(defun d-spec-simple-bindings (d-spec &optional form)
  (with-accessors ((var-spec var-spec)
                   (type-spec type-spec))
      d-spec
    (when var-spec
      `((,var-spec ,form)))))
