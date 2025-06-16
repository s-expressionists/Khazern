(asdf:defsystem "khazern-extension-intrinsic"
  :description "Khazern iteration path intrinsic extensions"
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern-intrinsic"
               "khazern-extension")
  :components ((:module code
                :pathname "code/extension/"
                :serial t
                :components ((:file "intrinsic")))))
