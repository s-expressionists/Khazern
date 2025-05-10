(asdf:defsystem :khazern-elements-extrinsic
  :description "Khazern ELEMENTS extrinsic path extension"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-extrinsic :khazern-elements)
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern-elements-extrinsic/test)))
  :components ((:module code
                :pathname "code/elements/"
                :serial t
                :components ((:file "extrinsic")))))

(asdf:defsystem :khazern-elements-extrinsic/test
  :description "Test system Khazern ELEMENTS extrinsic path extension"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-elements-extrinsic :parachute)
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :khazern-elements-extrinsic/test))
  :components ((:module code
                :pathname "code/elements/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
