(asdf:defsystem "khazern-intrinsic"
  :description "System for loading Khazern intrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern"
               "trivial-package-locks")
  :in-order-to ((asdf:test-op (asdf:test-op "khazern-intrinsic/test")))
  :components ((:module code
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "loop")))))

(asdf:defsystem "khazern-intrinsic/test"
  :description "ANSI Test system for Khazern"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("ansi-test-harness"
               "khazern-intrinsic")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :khazern-intrinsic/test :test))
  :components ((:module code
                :pathname "code/intrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))

