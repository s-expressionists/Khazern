(asdf:defsystem :khazern-stream-intrinsic
  :description "Khazern stream intrinsic path extensions"
  :license "BSD"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on (:khazern-intrinsic :khazern-stream)
  :components ((:module code
                :pathname "code/stream/"
                :serial t
                :components ((:file "intrinsic")))))
