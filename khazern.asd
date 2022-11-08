(cl:in-package #:asdf-user)

(defsystem :khazern
  :depends-on (:acclimation)
  :in-order-to ((asdf:test-op (asdf:test-op #:khazern/test)))
  :serial t
  :components ((:module code
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
  :serial t
  :components ((:module code
                :components ((:file "loop-defmacro")))))

(defsystem :khazern/intrinsic
  :depends-on (:khazern)
  :serial t
  :components ((:module code
                :components ((:file "loop-defmacro")))))

(defsystem :khazern/extrinsic
  :depends-on (:khazern)
  :serial t
  :components ((:module code
                :components ((:file "shadow-export")
                             (:file "loop-defmacro")))))

(defsystem :khazern/test
  :depends-on (:khazern)
  :perform (asdf:test-op (op c) (uiop:symbol-call :khazern/test :loop-test))
  :serial t
  :components ((:module test
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
