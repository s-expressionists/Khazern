(cl:in-package #:asdf-user)

(defsystem :khazern
  :description "A portable and extensible Common Lisp LOOP implementation"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:acclimation)
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern/ansi)))
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "compile-time-conditions")
                             (:file "run-time-conditions")
                             (:file "condition-reporters-english")
                             (:file "utilities")
                             (:file "combinatory-parsing")
                             (:file "parse-common")
                             (:file "clause")
                             (:file "expansion")
                             (:file "main-clause")
                             (:file "variable-clause")
                             (:file "selectable-clause")
                             (:file "unconditional-clause")
                             (:file "accumulation-clause")
                             (:file "termination-test-clause")
                             (:file "var-spec")
                             (:file "type-spec")
                             (:file "name-clause")
                             (:file "initial-clause")
                             (:file "final-clause")
                             (:file "with-clause")
                             (:file "return-clause")
                             (:file "do-clause")
                             (:file "collect-clause")
                             (:file "append-clause")
                             (:file "nconc-clause")
                             (:file "count-clause")
                             (:file "sum-clause")
                             (:file "maximize-clause")
                             (:file "minimize-clause")
                             (:file "conditional-clause")
                             (:file "while-until-clauses")
                             (:file "repeat-clause")
                             (:file "always-clause")
                             (:file "never-clause")
                             (:file "thereis-clause")
                             (:file "for-as-clause")
                             (:file "for-as-arithmetic-clause")
                             (:file "for-as-list-clause")
                             (:file "for-as-equals-then-clause")
                             (:file "for-as-across-clause")
                             (:file "for-as-hash-clause")
                             (:file "for-as-package-clause")
                             (:file "analysis")
                             (:file "run-time-support")
                             (:file "loop-defmacro")))))

(defsystem :khazern/environment
  :description "Khazern's initial environment contents for bootstrapping."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :components ((:module code
                :serial t
                :components ((:file "loop-defmacro")))))

(defsystem :khazern/intrinsic
  :description "System for loading Khazern intrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern :trivial-package-locks)
  :components ((:module code
                :serial t
                :components ((:file "intrinsic")))))

(defsystem :khazern/ansi
  :description "ANSI Test system for Khazern"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern/intrinsic)
  :perform (asdf:test-op (op c)
             (handler-case (uiop:symbol-call :khazern/ansi :test)
               (error (condition)
                 (format *error-output* "~%~%~a~%" condition)
                 (uiop:quit 100))
               (:no-error (result)
                 (uiop:quit))))
  :components ((:module ansi
                :serial t
                :components ((:file "packages")
                             (:file "test")))))

(defsystem :khazern/test
  :description "Test system for Khazern"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern :parachute)
  :perform (asdf:test-op (op c)
             (defparameter cl-user::*exit-on-test-failures* t)
             (uiop:symbol-call :parachute :test :khazern/test))
  :components ((:module test
                :serial t
                :components ((:file "packages")
                             (:file "test")))))
