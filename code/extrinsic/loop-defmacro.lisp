(in-package #:khazern-extrinsic)

(defmacro loop-finish ()
  '(go end-loop))

(defmacro loop (&rest forms)
  (if (every #'consp forms)
      (let ((tag (gensym)))
        `(block nil
           (tagbody
            ,tag
             ,@forms
             (go ,tag))))
      (khazern:expand-body forms 'end-loop)))
