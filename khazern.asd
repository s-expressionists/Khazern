(asdf:defsystem "khazern"
  :description "A portable and extensible Common Lisp LOOP implementation"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("acclimation"
               "trivial-with-current-source-form")
  :components ((:module code
                :serial t
                :components ((:file "packages")
                             (:file "interface")
                             (:file "utilities")
                             (:file "types")
                             (:file "conditions")
                             (:file "conditions-english")
                             (:file "binding")
                             (:file "parser")
                             (:file "clause")
                             (:file "expansion")
                             (:file "variable-init-and-stepping-clauses")
                             (:file "value-accumulation-clauses")
                             (:file "termination-test-clauses")
                             (:file "unconditional-execution-clauses")
                             (:file "conditional-execution-clauses")
                             (:file "miscellaneous-clauses")))))
