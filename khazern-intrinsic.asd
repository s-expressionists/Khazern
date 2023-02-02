(asdf:defsystem :khazern-intrinsic/environment
  :description "Khazern's initial environment contents for bootstrapping."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :components ((:module code
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "loop-defmacro")))))

(asdf:defsystem :khazern-intrinsic
  :description "System for loading Khazern intrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern #:trivial-package-locks)
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern-intrinsic/test)))
  :components ((:module code
                :pathname "code/intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "loop-defmacro")))))

(asdf:defsystem :khazern-intrinsic/test
  :description "ANSI Test system for Khazern"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-intrinsic)
  :perform (asdf:test-op (op c)
             (handler-case (uiop:symbol-call :khazern-intrinsic/test :test)
               (error (condition)
                 (format *error-output* "~%~%~a~%" condition)
                 (uiop:quit 100))
               (:no-error (result)
                 (declare (ignore result))
                 (uiop:quit))))
  :components ((:module code
                :pathname "code/intrinsic/test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))

