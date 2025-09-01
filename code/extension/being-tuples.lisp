(cl:in-package #:khazern-extension)

(defclass being-tuples (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%of-ref :accessor of-ref)
   (%result-type :accessor result-type)
   (%iter-ref :accessor iter-ref)
   (%limit-ref :accessor limit-ref)
   (%len-ref :accessor len-ref)))

(defmethod initialize-instance :after ((instance being-tuples) &rest initargs &key)
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
  
(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :tuple)) &key var)
  (make-instance 'being-tuples :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :tuples)) &key var)
  (make-instance 'being-tuples :var var :start khazern:*start*))

(defmethod khazern:preposition-names
    ((client extension-client) (instance being-tuples))
  (values '((:in :of))
          '((:in :of))
          '()))

(defun parse-being-tuples-of (instance)
  (setf (of-ref instance) (khazern:add-simple-binding instance
                                                      :var "OF"
                                                      :form (khazern:parse-token)
                                                      :type 'sequence)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-tuples) (key (eql :in)))
  (parse-being-tuples-of instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-tuples) (key (eql :of)))
  (parse-being-tuples-of instance))

(defmethod khazern:analyze ((client extension-client) (instance being-tuples))
  (if (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
      (setf (khazern:type-spec (var instance)) (if (consp (khazern:var-spec (var instance)))
                                                   t
                                                   'sequence)
            (result-type instance) `(type-of ,(of-ref instance)))
      (setf (result-type instance) `',(khazern:type-spec (var instance)))))

(defmethod khazern:step-intro-forms ((clause being-tuples) initialp)
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

(defmethod khazern:step-outro-forms ((clause being-tuples) initialp)
  (declare (ignore initialp))
  (with-accessors ((iter-ref iter-ref)
                   (of-ref of-ref)
                   (len-ref len-ref)
                   (result-type result-type))
      clause
    (khazern:expand-assignments (var clause)
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
