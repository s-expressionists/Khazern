(cl:in-package #:khazern-sequence-extrinsic/test)

(define-test loop-over-vector
  (is equal
      '(a b c)
      (khazern-extrinsic:loop for i over #(a b c)
                              collect i)))

(define-test loop-over-at-vector
  (is equal
      '(a 0 b 1 c 2)
      (khazern-extrinsic:loop for i at j over #(a b c)
                              collect i
                              collect j)))
