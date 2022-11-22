(cl:in-package #:khazern/test)

(define-test loop-finish-in-simple-loop
  (is equal
      (khazern:loop do (khazern:loop (khazern:loop-finish)))
      nil))

(define-test loop-until-t
  (is equal
      (khazern:loop until t)
      nil))

(define-test loop-while-nil
  (is equal
      (khazern:loop while nil)
      nil))

(define-test loop-until-expr
  (is equal
      (let ((n 0))
        (khazern:loop until (= (incf n) 10))
        n)
      10))

(define-test loop-while-expr
  (is equal
      (let ((n 0))
        (khazern:loop while (< (incf n) 10))
        n)
      10))

(define-test loop-do
  (is equal
      (let ((n 0))
        (block abc
          (khazern:loop do (progn (incf n)
                             (when (= n 10)
                               (return-from abc nil)))))
        n)
      10))
