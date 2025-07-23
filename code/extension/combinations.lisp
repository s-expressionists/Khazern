(cl:in-package #:khazern-extension)

(defclass for-as-combinations (khazern:clause)
  ((%var :accessor var
         :initarg :var)
   (%result-type :accessor result-type)
   (%of-ref :accessor of-ref)
   (%comb-ref :accessor comb-ref)
   (%choose-ref :accessor choose-ref)
   (%len-ref :accessor len-ref)
   (%pos-ref :accessor pos-ref)))

(defmethod initialize-instance :after ((instance for-as-combinations) &rest initargs &key)
  (declare (ignore initargs))
  (khazern:add-binding instance (var instance))
  (setf (comb-ref instance) (khazern:add-simple-binding instance
                                                        :var "COMB"
                                                        :form '(make-array 0
                                                                :element-type 'fixnum)
                                                        :type '(vector fixnum))
        (len-ref instance) (khazern:add-simple-binding instance
                                                       :var "LEN" :type 'fixnum)
        (pos-ref instance) (khazern:add-simple-binding instance
                                                       :var "POS" :type 'fixnum)))
  
(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :combination)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-combinations :var var)))

(defmethod khazern:make-iteration-path
    ((client extension-client) (name (eql :combinations)) var
     &optional (inclusive-form nil inclusive-form-p))
  (declare (ignore inclusive-form))
  (if inclusive-form-p
      (call-next-method)
      (make-instance 'for-as-combinations :var var)))

(defmethod khazern:preposition-names ((client extension-client) (instance for-as-combinations))
  (values '((:in :of) :choose)
          '((:in :of) :choose)
          '()))

(defun parse-of-preposition (instance)
  (setf (of-ref instance) (khazern:add-simple-binding instance
                                                      :var "OF"
                                                      :form (khazern:parse-token)
                                                      :type 'sequence)))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-combinations) (key (eql :in)))
  (parse-of-preposition instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-combinations) (key (eql :of)))
  (parse-of-preposition instance))

(defmethod khazern:parse-preposition
    ((client extension-client) (instance for-as-combinations) (key (eql :choose)))
  (setf (choose-ref instance) (khazern:add-simple-binding instance
                                                          :var "CHOOSE"
                                                          :form (khazern:parse-token)
                                                          :type 'fixnum)))

(defmethod khazern:analyze ((client extension-client) (instance for-as-combinations))
  (if (eq (khazern:type-spec (var instance)) khazern:*placeholder-result*)
      (setf (khazern:type-spec (var instance)) 'sequence
            (result-type instance) `(class-of ,(of-ref instance)))
      (setf (result-type instance) `',(khazern:type-spec (var instance)))))

(defmethod khazern:step-intro-forms ((clause for-as-combinations) initialp)
  (with-accessors ((comb-ref comb-ref)
                   (choose-ref choose-ref)
                   (len-ref len-ref)
                   (pos-ref pos-ref)
                   (of-ref of-ref))
      clause
    (if initialp
        `((setq ,len-ref (length ,of-ref)
                ,comb-ref (make-array ,choose-ref :element-type 'fixnum))
          (prog ((pos ,choose-ref))
           next
             (when (plusp pos)
               (decf pos)
               (setf (aref ,comb-ref pos) pos)
               (go next))))
        (let ((next1-tag (gensym (symbol-name :next)))
              (next2-tag (gensym (symbol-name :next))))
          `(  (setq ,pos-ref (1- ,choose-ref))
            ,next1-tag
              (when (minusp ,pos-ref)
                (go ,khazern:*epilogue-tag*))
              (unless (or (and (= (1+ ,pos-ref) ,choose-ref)
                               (< (1+ (aref ,comb-ref ,pos-ref)) ,len-ref))
                          (and (< (1+ ,pos-ref) ,choose-ref)
                               (< (1+ (aref ,comb-ref ,pos-ref))
                                  (aref ,comb-ref (1+ ,pos-ref)))))
                (decf ,pos-ref)
                (go ,next1-tag))
              (incf (aref ,comb-ref ,pos-ref))
              (incf ,pos-ref)
            ,next2-tag
              (when (< ,pos-ref ,choose-ref)
                (setf (aref ,comb-ref ,pos-ref) (1+ (aref ,comb-ref (1- ,pos-ref))))
                (incf ,pos-ref)
                (go ,next2-tag)))))))

(defmethod khazern:step-outro-forms ((clause for-as-combinations) initialp)
  (declare (ignore initialp))
  (with-accessors ((comb-ref comb-ref)
                   (of-ref of-ref)
                   (result-type result-type))
      clause
    (khazern:destructuring-set (var clause) `(map ,result-type
                                                  (lambda (pos)
                                                    (elt ,of-ref pos))
                                                  ,comb-ref))))
