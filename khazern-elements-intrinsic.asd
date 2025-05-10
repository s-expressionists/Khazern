(asdf:defsystem :khazern-elements-intrinsic
  :description "Khazern ELEMENTS intrinsic path extension"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-intrinsic :khazern-elements)
  :components ((:module code
                :pathname "code/elements/"
                :serial t
                :components ((:file "intrinsic")))))
