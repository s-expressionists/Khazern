(in-package #:khazern-intrinsic/test)

(defun test ()
  (let ((system (asdf:find-system :khazern-intrinsic/test)))
    (ansi-test-harness:ansi-test
     :exit t
     :directory (merge-pathnames (make-pathname
                                  :directory '(:relative "dependencies" "ansi-test"))
                                 (asdf:component-pathname system))
     :tests '("LOOP"))))
