(asdf:defsystem "khazern-extension-extrinsic"
  :description "Khazern iteration path extrinsic extensions"
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern-extrinsic"
               "khazern-extension")
  :in-order-to ((asdf:test-op (asdf:test-op "khazern-extension-extrinsic/test")))
  :components ((:module code
                :pathname "code/extension/"
                :serial t
                :components ((:file "extrinsic")))))

(asdf:defsystem "khazern-extension-extrinsic/test"
  :description "Test system for Khazern iteration path extrinsic extensions"
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern-extension-extrinsic"
               "nontrivial-gray-streams"
               "parachute")
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :khazern-extension-extrinsic/test))
  :components ((:module code
                :pathname "code/extension/test/"
                :serial t
                :components ((:file "packages")
                             (:file "utility")
                             (:file "elements")
                             (:file "stream")))))
