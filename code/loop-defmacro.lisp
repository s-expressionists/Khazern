(cl:in-package #:khazern)

(defmacro loop-finish ()
  '(go end-loop))

#+(or)(defmacro loop (&rest forms)
  (if (every #'consp forms)
    (let ((tag (gensym)))
      `(block nil
         (tagbody
           ,tag
           ,@forms
           (go ,tag))))
    (let ((end-tag (gensym)))
      `(macrolet ((loop-finish ()
                    `(go ,',end-tag)))
         ,(expand-body forms end-tag)))))

(defmacro loop (&rest forms)
  (if (every #'consp forms)
      (let ((tag (gensym)))
        `(block nil
           (tagbody
            ,tag
             ,@forms
             (go ,tag))))
      (expand-body forms 'end-loop)))
