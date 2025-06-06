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

(defun random-double-floats (count)
  (loop repeat count
        collect (if (zerop (random 2))
                    (- (random 1000d0))
                    (random 1000d0))))

(defvar *random-double-floats*
  (random-double-floats 100))

(define-benchmark expansion
  (tma:macroexpand-all
    '(loop for i in '(1 324 2345 323 2 4 235 252)
           when (oddp i)
             do (print i)
             and collect i into odd-numbers
             and do (terpri)
           else
             collect i into even-numbers
           finally (return (values odd-numbers even-numbers)))))

(define-benchmark repeat
  (loop repeat 100
        do (nothing)))

(define-benchmark repeat/collect
  (loop repeat 100
        collect 1))

(define-benchmark repeat/nconc
  (loop repeat 100
        nconc (list 1 2 3)))

(define-benchmark repeat/append
  (loop repeat 100
        append '(1 2 3)))

(define-benchmark for-as-in-list/maximize
  (loop for i in *random-double-floats*
        maximize i))

(define-benchmark for-as-in-list/minimize
  (loop for i in *random-double-floats*
        maximize i))

(define-benchmark for-as-in-list/sum
  (loop for i in *random-double-floats*
        sum i))

(define-benchmark for-as-in-list/destructure
  (loop for (a b (c d) . e) in *random-trees*
        do (nothing)))

(define-benchmark for-as-equals-then/destructure/repeat
  (loop for (a b (c d) . e) = *random-trees*
        repeat 100
        do (nothing)))

(define-benchmark for-as-arithmetic
  (loop for i below 100
        do (nothing)))

(define-benchmark for-as-across
  (loop for i across #(0 1 2 3 4 5 6 7 8 9)
        do (nothing)))
