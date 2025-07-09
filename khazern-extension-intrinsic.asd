(asdf:defsystem "khazern-extension-intrinsic"
  :description "Khazern iteration path intrinsic extensions"
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern-extension"
               "trivial-package-locks")
  :components ((:module code
                :pathname "code/extension-intrinsic/"
                :serial t
                :components ((:file "packages")
                             (:file "loop")))))
