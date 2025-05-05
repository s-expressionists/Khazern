(cl:in-package #:khazern-extrinsic/benchmark)

(defun nothing ())

(defun random-tree (depth)
  (if (zerop (random (1+ depth)))
      nil
      (cons (random-tree (1- depth))
            (random-tree (1- depth)))))

(defvar *random-trees*
  (loop repeat 100
        collect (random-tree 5)))

(define-benchmark repeat
  (loop repeat 100
        do (nothing)))

(define-benchmark repeat/collect
  (loop repeat 100
        collect 1))

(define-benchmark for-as-list/destructure
  (loop for (a b (c d) . e) in *random-trees*
        do (nothing)))

(define-benchmark for-as-equals-then/destructure/repeat
  (loop for (a b (c d) . e) = *random-trees*
        repeat 100
        do (nothing)))
