(asdf:defsystem :khazern-extrinsic
  :description "System for loading Khazern extrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern)
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern-extrinsiczz/test)))
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "loop-defmacro")))))

(defsystem :khazern-extrinsic/test
  :description "Test system for Khazern"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-extrinsic :parachute)
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :khazern-extrinsic/test))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
