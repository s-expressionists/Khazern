(in-package #:khazern-extrinsic/ansi-test)

(defun test ()
  (let ((system (asdf:find-system :khazern-extrinsic/test)))
    (ansi-test-harness:ansi-test
     :directory (merge-pathnames (make-pathname
                                  :directory '(:relative "dependencies" "ansi-test"))
                                 (asdf:component-pathname system))
     :extrinsic-symbols '(khazern-extrinsic:loop-finish
                          khazern-extrinsic:loop)
     :tests '("LOOP"))))
