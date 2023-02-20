(cl:in-package #:khazern-intrinsic)

(trivial-package-locks:with-unlocked-packages (:common-lisp)

(defmacro cl:loop-finish ()
  '(go end-loop))

(defmacro loop (&rest forms)
  (if (every #'consp forms)
      (let ((tag (gensym)))
        `(block nil
           (tagbody
            ,tag
             ,@forms
             (go ,tag))))
      (khazern:expand-body forms 'end-loop))))
