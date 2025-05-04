(asdf:defsystem "khazern-extrinsic"
  :description "System for loading Khazern extrinsically into an implementation."
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern")
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern-extrinsic/test)))
  :components ((:module code
                :pathname "code/extrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "loop-defmacro")))))

(asdf:defsystem "khazern-extrinsic/test"
  :description "Test system for Khazern"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("ansi-test-harness"
               "khazern-extrinsic"
               "parachute")
  :perform (asdf:test-op (op c)
             (let* ((regression (uiop:symbol-call :parachute :test :khazern-extrinsic/test))
                    (ansi (uiop:symbol-call :khazern-extrinsic/ansi-test :test)))
               (uiop:quit (if (and regression ansi) 0 100))))
  :components ((:module code
                :pathname "code/extrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")
                             (:file "ansi-test")))))

(asdf:defsystem "khazern-extrinsic/benchmark"
  :description "Benchmark system for Khazern"
  :license "MIT"
  :author ("Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :source-control (:git "https://github.com/s-expressionists/Khazern.git")
  :depends-on ("cl-spark"
               "cl-ascii-table"
               "khazern-extrinsic")
  :components ((:module "code"
                :pathname "code/extrinsic/benchmark/"
                :serial t
                :components ((:file "packages")
                             (:file "utility")
                             (:file "benchmarks")))))
