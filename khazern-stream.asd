(asdf:defsystem "khazern-stream"
  :description "Khazern stream path extensions"
  :license "BSD"
  :author ("Tarn W. Burton")
  :maintainer "Tarn W. Burton"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Khazern"
  :bug-tracker "https://github.com/s-expressionists/Khazern/issues"
  :depends-on ("khazern")
  :components ((:module code
                :pathname "code/stream/"
                :serial t
                :components ((:file "packages")
                             (:file "stream")))))
