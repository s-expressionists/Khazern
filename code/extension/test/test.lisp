(cl:in-package #:khazern-extension-extrinsic/test)

(define-test loop-vector-01
  (is equal
      '(a b c)
      (khazern-extrinsic:loop for i being the elements
                                in #(a b c)
                              collect i)))

(define-test loop-vector-02
  (is equal
      '(a 0 b 1 c 2)
      (khazern-extrinsic:loop for i being the elements
                                in #(a b c)
                                using (index j)
                              collect i
                              collect j)))
