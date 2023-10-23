(in-package #:khazern-extrinsic/ansi-test)

(defun test ()
  (let ((system (asdf:find-system :khazern-extrinsic/test)))
    (ansi-test-harness:ansi-test :directory (merge-pathnames (make-pathname :directory '(:relative "dependencies" "ansi-test"))
                                                             (asdf:component-pathname system))
                                 ;:expected-failures (asdf:component-pathname (asdf:find-component system
                                 ;                                                                 '("code" "expected-failures.sexp")))
                                 :extrinsic-symbols '(khazern-extrinsic:loop-finish
                                                      khazern-extrinsic:loop)
                                 :tests '("LOOP"))))
