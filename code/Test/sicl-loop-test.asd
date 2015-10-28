(cl:in-package #:asdf-user)

(defsystem :sicl-loop-test
  :depends-on (:sicl-loop-support)
  :serial t
  :components
  ((:file "test-packages")
   (:file "loop-defmacro")
   (:file "loop-test")
   (:file "simple-loop")
   (:file "loop1")
   (:file "loop2")
   (:file "loop3")
   (:file "loop4")
   (:file "loop5")
   (:file "loop6")
   (:file "loop7")
   (:file "loop8")
   (:file "loop9")
   (:file "loop10")
   (:file "loop11")
   (:file "loop12")
   (:file "loop13")
   (:file "loop14")
   (:file "loop15")
   (:file "loop16")
   (:file "loop17")))
