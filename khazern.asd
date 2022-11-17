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
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern/test)))
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
                             (:file "run-time-support")))))

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
  :depends-on (:khazern)
  :components ((:module code
                :serial t
                :components ((:file "loop-defmacro")))))

(defsystem :khazern/extrinsic
  :description "System for loading Khazern extrinsically into an implementation."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern)
  :components ((:module code
                :serial t
                :components ((:file "shadow-export")
                             (:file "loop-defmacro")))))

(defsystem :khazern/test
  :description "Test system for Khazern"
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern)
  :perform (asdf:test-op (op c) (uiop:symbol-call :khazern/test :loop-test))
  :components ((:module test
                :serial t
                :components ((:file "test-packages")
                             (:file "loop-defmacro")
                             (:file "loop-test")
                             (:file "simple-loop")
                             (:file "loop1")
                             (:file "loop2")
                             (:file "loop3")
                             (:file "loop4")
                             (:file "loop5")
                             (:file "loop6")
                             (:file "loop7")
                             (:file "loop8")
                             (:file "loop9")
                             (:file "loop10")
                             (:file "loop11")
                             (:file "loop12")
                             (:file "loop13")
                             (:file "loop14")
                             (:file "loop15")
                             (:file "loop16")
                             (:file "loop17")))))
