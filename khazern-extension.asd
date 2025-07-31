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
                             (:file "being-elements")
                             (:file "being-stream-values")
                             (:file "being-entries")
                             (:file "being-hash-entries")
                             (:file "being-package-symbols")
                             (:file "being-combinations")
                             (:file "being-permutations")
                             (:file "being-tuples")
                             (:file "cleanup-clause")
                             (:file "sequence-accumulation")
                             (:file "use-clause")))))
