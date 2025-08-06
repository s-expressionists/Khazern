(cl:in-package #:khazern-extension)

(defclass being-permutations (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%of-ref :accessor of-ref)
   (%result-type :accessor result-type)
   (%perm-ref :accessor perm-ref)
   (%state-ref :accessor state-ref)
   (%pos-ref :accessor pos-ref)
   (%len-ref :accessor len-ref)))

(defmethod initialize-instance :after ((instance being-permutations) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance))
  (setf (perm-ref instance) (khazern:add-simple-binding instance
                                                        :var "PERM"
                                                        :form '(make-array 0
                                                                :element-type 'fixnum)
                                                        :type '(vector fixnum))
        (state-ref instance) (khazern:add-simple-binding instance
                                                         :var "STATE"
                                                         :form '(make-array 0
                                                                 :element-type 'fixnum)
                                                         :type '(vector fixnum))
        (pos-ref instance) (khazern:add-simple-binding instance
                                                       :var "POS" :type 'fixnum :form 1)
        (len-ref instance) (khazern:add-simple-binding instance
                                                       :var "LEN" :type 'fixnum)))
  
(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :permutation)) &key var)
  (make-instance 'being-permutations :var var :start khazern:*start*))

(defmethod khazern:parse-clause
    ((client extension-client) (region khazern:being-region) (name (eql :permutations))
     &key var)
  (make-instance 'being-permutations :var var :start khazern:*start*))

(defmethod khazern:preposition-names ((client extension-client) (instance being-permutations))
  (values '((:in :of))
          '((:in :of))
          '()))

(defun parse-being-permutations-of (instance)
  (setf (of-ref instance) (khazern:add-simple-binding instance
                                                      :var "OF"
                                                      :form (khazern:parse-token)
                                                      :type 'sequence)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-permutations) (key (eql :in)))
  (parse-being-permutations-of instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance being-permutations) (key (eql :of)))
  (parse-being-permutations-of instance))

(defmethod khazern:analyze ((client extension-client) (instance being-permutations))
  (if (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
      (setf (khazern:type-spec (var instance)) 'sequence
            (result-type instance) `(type-of ,(of-ref instance)))
      (setf (result-type instance) `',(khazern:type-spec (var instance)))))

(defmethod khazern:step-intro-forms ((clause being-permutations) initialp)
  (with-accessors ((perm-ref perm-ref)
                   (state-ref state-ref)
                   (len-ref len-ref)
                   (of-ref of-ref)
                   (pos-ref pos-ref))
      clause
    (if initialp
        `((setq ,len-ref (length ,of-ref)
                ,perm-ref (make-array ,len-ref :element-type 'fixnum)
                ,state-ref (make-array ,len-ref :element-type 'fixnum))
          (prog ((pos ,len-ref))
           next
             (when (plusp pos)
               (decf pos)
               (setf (aref ,perm-ref pos) pos)
               (go next))))
        (let ((next-tag (gensym "NEXT")))
          `(,next-tag
              (unless (< ,pos-ref ,len-ref)
                (go ,khazern:*epilogue-tag*))
              (unless (< (aref ,state-ref ,pos-ref) ,pos-ref)
                (setf (aref ,state-ref ,pos-ref) 0)
                (incf ,pos-ref)
                (go ,next-tag))
              (let ((temp (aref ,perm-ref ,pos-ref))
                    (pos (if (zerop (mod ,pos-ref 2))
                             0
                             (aref ,state-ref ,pos-ref))))
                (setf (aref ,perm-ref ,pos-ref) (aref ,perm-ref pos)
                      (aref ,perm-ref pos) temp))
              (incf (aref ,state-ref ,pos-ref))
              (setq ,pos-ref 1))))))

(defmethod khazern:step-outro-forms ((clause being-permutations) initialp)
  (declare (ignore initialp))
  (with-accessors ((perm-ref perm-ref)
                   (of-ref of-ref)
                   (result-type result-type))
      clause
    (khazern:expand-assignments (var clause) `(map ,result-type
                                                   (lambda (pos)
                                                     (elt ,of-ref pos))
                                                   ,perm-ref))))
