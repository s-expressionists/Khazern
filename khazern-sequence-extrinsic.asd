(asdf:defsystem :khazern-sequence-extrinsic
  :description "Khazern LOOP extension for extensible sequence protocol"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-extrinsic :khazern-sequence)
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern-sequence-extrinsic/test)))
  :components ((:module code
                :pathname "code/sequence/"
                :serial t
                :components ((:file "extrinsic")))))

(asdf:defsystem :khazern-sequence-extrinsic/test
  :description "Test system for Khazern"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-sequence-extrinsic :parachute)
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :khazern-sequence-extrinsic/test))
  :components ((:module code
                :pathname "code/sequence/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
