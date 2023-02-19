(asdf:defsystem :khazern-environment
  :description "Khazern's initial environment contents for bootstrapping."
  :license "BSD"
  :author "Robert Strandh"
  :maintainer "Robert Strandh"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :components ((:module code
                :pathname "code/environment/"
                :serial t
                :components ((:file "loop-defmacro")))))
