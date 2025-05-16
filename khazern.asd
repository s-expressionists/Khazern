(asdf:defsystem :khazern
  :description "A portable and extensible Common Lisp LOOP implementation"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:acclimation)
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "compile-time-conditions")
                             (:file "run-time-conditions")
                             (:file "condition-reporters-english")
                             (:file "utilities")
                             (:file "interface")
                             (:file "parser-table")
                             (:file "combinatory-parsing")
                             (:file "parse-common")
                             (:file "clause")
                             (:file "expansion")
                             (:file "var-spec")
                             (:file "type-spec")
                             (:file "variable-init-and-stepping-clauses")
                             (:file "value-accumulation-clauses")
                             (:file "termination-test-clauses")
                             (:file "unconditional-execution-clauses")
                             (:file "conditional-execution-clauses")
                             (:file "miscellaneous-clauses")
                             (:file "default-table")
                             (:file "analysis")))))
