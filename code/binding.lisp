(cl:in-package #:khazern)

(defclass binding ()
  ((%var-spec :accessor var-spec
              :initarg :var-spec)
   (%type-spec :accessor type-spec
               :initarg :type-spec
               :initform t)
   (%ignorable :accessor ignorablep
               :initarg :ignorable
               :initform nil)
   (%dynamic-extent :accessor dynamic-extent-p
                    :initarg :dynamic-extent
                    :initform nil)))

(defclass simple-binding (binding)
  ((%category :accessor category
                           :initarg :category
                           :initform nil)
   (%scope-references :accessor scope-references
                             :initarg :scope-references
                             :initform nil)
   (%form :accessor form
          :initarg :form
          :initform nil)))

(defclass destructuring-binding (binding)
  ((%temps :accessor temps)))

(defun set-destructuring-temps (binding)
  (let ((temps (make-hash-table)))
    (labels ((traverse (d-var-spec)
               (cond ((null d-var-spec)
                      nil)
                     ((symbolp d-var-spec)
                      t)
                     (t
                      (let ((car-p (traverse (car d-var-spec)))
                            (cdr-p (traverse (cdr d-var-spec))))
                        (when (and car-p cdr-p)
                          (setf (gethash d-var-spec temps) (gensym "DE")))
                        (or car-p cdr-p))))))
      (traverse (var-spec binding))
      (setf (temps binding) temps))))

(defmethod initialize-instance :after ((clause destructuring-binding) &rest initargs &key)
  (declare (ignore initargs))
  (set-destructuring-temps clause))

(defun make-destructuring-binding (spec
                                   &key (type t) ((:ignorable ignorablep) nil)
                                        ((:dynamic-extent dynamic-extent-p) nil))
  "Make a destructuring binding."
  (make-instance 'destructuring-binding
                 :var-spec spec
                 :type-spec type
                 :ignorable ignorablep
                 :dynamic-extent dynamic-extent-p))

(defun destructuring-set (binding form)
  "Return the SETQ for a destructuring binding and a form."
  (let ((assignments '())
        (temps (temps binding)))
    (labels ((traverse (d-var-spec form)
               (cond ((null d-var-spec))
                     ((symbolp d-var-spec)
                      (push d-var-spec assignments)
                      (push form assignments))
                     (t
                      (let ((temp (gethash d-var-spec temps)))
                        (cond (temp
                               (push temp assignments)
                               (push form assignments)
                               (traverse (car d-var-spec) `(car ,temp))
                               (traverse (cdr d-var-spec) `(cdr ,temp)))
                              (t
                               (traverse (car d-var-spec) `(car ,form))
                               (traverse (cdr d-var-spec) `(cdr ,form)))))))))
      (traverse (var-spec binding) form)
      (when assignments
        `((setq ,.(nreverse assignments)))))))

(defmethod map-variables progn (function (binding binding))
  (labels ((traverse (var-spec type-spec)
             (cond ((null var-spec))
                   ((symbolp var-spec)
                    (funcall function var-spec (or type-spec t)
                             (category binding)
                             (scope-references binding)))
                   ((symbolp type-spec)
                    (traverse (car var-spec) type-spec)
                    (traverse (cdr var-spec) type-spec))
                   (t
                    (traverse (car var-spec) (car type-spec))
                    (traverse (cdr var-spec) (cdr type-spec))))
             nil))
    (traverse (var-spec binding) (type-spec binding))))

(defun check-type-spec (instance)
  "Check the types and initial values of a destructuring binding."
  (labels ((deduce (type-spec)
             (cond ((nth-value 1 (deduce-initial-value type-spec))
                    type-spec)
                   (t
                    (warn 'unable-to-deduce-initial-value
                          :type-spec type-spec)
                    `(or ,(if (subtypep type-spec 'number)
                              'bit
                              'null)
                         ,type-spec))))
           (traverse (var-spec type-spec)
             (cond ((null var-spec)
                    nil)
                   ((symbolp var-spec)
                    (deduce type-spec))
                   ((symbolp type-spec)
                    (setf type-spec (deduce type-spec))
                    (cons (traverse (car var-spec) type-spec)
                          (traverse (cdr var-spec) type-spec)))
                   (t
                    (cons (traverse (car var-spec) (car type-spec))
                          (traverse (cdr var-spec) (cdr type-spec)))))))
    (setf (type-spec instance)
          (traverse (var-spec instance) (type-spec instance)))))

(defmethod declarations ((binding destructuring-binding))
  (let ((result '())
        (variables '()))
    (map-variables (lambda (var type category references)
                     (declare (ignore category references))
                     (unless (eq type t)
                       (push `(type ,type ,var) result))
                     (push var variables))
                   binding)
    (setf variables (nreverse variables))
    (when (ignorablep binding)
      (push `(ignorable ,@variables) result))
    (when (dynamic-extent-p binding)
      (push `(dynamic-extent ,@variables) result))
    (nreverse result)))

(defmethod variable-list ((binding destructuring-binding))
  (let ((result '()))
    (map-variables (lambda (var type category references)
                     (declare (ignore category references))
                     (push (list var (deduce-initial-value type))
                           result))
                   binding)
    (maphash (lambda (object temp)
               (declare (ignore object))
               (push `(,temp nil) result))
             (temps binding))
    (nreverse result)))

(defmethod declarations ((binding simple-binding))
  (with-accessors ((var-spec var-spec)
                   (type-spec type-spec)
                   (ignorablep ignorablep)
                   (dynamic-extent-p dynamic-extent-p))
      binding
    (let (decl)
      (when var-spec
        (unless (eq type-spec t)
          (push `(type ,type-spec ,var-spec)
                decl))
        (when ignorablep
          (push `(ignorable ,var-spec) decl))
        (when dynamic-extent-p
          (push `(dynamic-extent ,var-spec) decl)))
      decl)))

(defmethod variable-list ((binding simple-binding))
  (with-accessors ((var-spec var-spec)
                   (form form))
      binding
    (when var-spec
      `((,var-spec ,form)))))

(defun simple-set (&rest pairs)
  (prog ((head pairs)
         result)
   next
     (cond ((null head)
            (return (when result
                      `((setq ,@(nreverse result))))))
           ((car head)
            (push (first head) result)
            (push (second head) result)))
     (setq head (cddr head))
     (go next)))
