(asdf:defsystem "khazern-extension"
  :description "Khazern iteration path extensions"
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern")
  :components ((:module code
                :pathname "code/extension/"
                :serial t
                :components ((:file "packages")
                             (:file "interface")
                             (:file "elements")
                             (:file "entries")
                             (:file "stream")
                             (:file "for-as-hash")
                             (:file "permutations")
                             (:file "combinations")
                             (:file "tuples")
                             (:file "cleanup")
                             (:file "sequence-accumulation")
                             (:file "use-clause")))))
