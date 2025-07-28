(cl:in-package #:khazern-extension)

(defclass for-as-tuple (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%of-ref :accessor of-ref)
   (%result-type :accessor result-type)
   (%iter-ref :accessor iter-ref)
   (%limit-ref :accessor limit-ref)
   (%len-ref :accessor len-ref)))

(defmethod initialize-instance :after ((instance for-as-tuple) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance))
  (setf (iter-ref instance) (khazern:add-simple-binding instance
                                                        :var "ITER"
                                                        :type 'fixnum)
        (limit-ref instance) (khazern:add-simple-binding instance
                                                         :var "LIMIT"
                                                         :type 'fixnum)
        (len-ref instance) (khazern:add-simple-binding instance
                                                       :var "LEN"
                                                       :type 'list)))
  
(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :tuple)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-tuple :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :tuples)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-tuple :var var)))

(defmethod khazern:preposition-names
    ((client extension-client) (instance for-as-tuple))
  (values '((:in :of))
          '((:in :of))
          '()))

(defun parse-of-preposition (instance)
  (setf (of-ref instance) (khazern:add-simple-binding instance
                                                      :var "OF"
                                                      :form (khazern:parse-token)
                                                      :type 'sequence)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-tuple) (key (eql :in)))
  (parse-of-preposition instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-tuple) (key (eql :of)))
  (parse-of-preposition instance))

(defmethod khazern:analyze ((client extension-client) (instance for-as-tuple))
  (if (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
      (setf (khazern:type-spec (var instance)) 'sequence
            (result-type instance) `(type-of ,(of-ref instance)))
      (setf (result-type instance) `',(khazern:type-spec (var instance)))))

(defmethod khazern:step-intro-forms ((clause for-as-tuple) initialp)
  (with-accessors ((iter-ref iter-ref)
                   (len-ref len-ref)
                   (of-ref of-ref)
                   (limit-ref limit-ref))
      clause
    (nconc (if initialp
               `((setq ,len-ref (map 'list
                                     (lambda (seq-or-len)
                                       (if (numberp seq-or-len)
                                           seq-or-len
                                           (length seq-or-len)))
                                     ,of-ref)
                      ,limit-ref (apply #'* ,len-ref))
                (mapl (lambda (len)
                        (rplaca len (apply #'* (cdr len))))
                      ,len-ref))
              `((incf ,iter-ref)))
          `((unless (< ,iter-ref ,limit-ref)
              (go ,khazern:*epilogue-tag*))))))

(defmethod khazern:step-outro-forms ((clause for-as-tuple) initialp)
  (declare (ignore initialp))
  (with-accessors ((iter-ref iter-ref)
                   (of-ref of-ref)
                   (len-ref len-ref)
                   (result-type result-type))
      clause
    (khazern:destructuring-set (var clause)
                               `(let ((iter-tmp ,iter-ref))
                                  (map ,result-type
                                       (lambda (arr-or-len div)
                                         (multiple-value-bind (q r)
                                             (floor iter-tmp div)
                                           (setq iter-tmp r)
                                           (if (numberp arr-or-len)
                                               q
                                               (elt arr-or-len q))))
                                       ,of-ref ,len-ref)))))
