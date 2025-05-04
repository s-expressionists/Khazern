(cl:in-package #:khazern-extrinsic/benchmark)

(defun nothing ())

(define-benchmark repeat
  (loop repeat 100
        do (nothing)))

(define-benchmark repeat/collect
  (loop repeat 100
        collect 1))
