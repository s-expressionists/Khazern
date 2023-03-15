(asdf:defsystem :khazern-sequence-intrinsic
  :description "Khazern LOOP extension for extensible sequence protocol"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-intrinsic :khazern-sequence)
  :components ((:module code
                :pathname "code/sequence/"
                :serial t
                :components ((:file "intrinsic")))))
