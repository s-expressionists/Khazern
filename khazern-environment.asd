(asdf:defsystem :khazern-environment
  :description "Khazern's initial environment contents for bootstrapping."
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :components ((:module code
                :pathname "code/environment/"
                :serial t
                :components ((:file "loop-defmacro")))))
