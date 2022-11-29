(cl:in-package #:khazern/test)

(define-test loop-finish-in-simple-loop
  (is equal
      nil
      (khazern:loop do (khazern:loop (khazern:loop-finish)))))

(define-test loop-until-t
  (is equal
      nil
      (khazern:loop until t)))

(define-test loop-while-nil
  (is equal
      nil
      (khazern:loop while nil)))

(define-test loop-until-expr
  (is equal
      10
      (let ((n 0))
        (khazern:loop until (= (incf n) 10))
        n)))

(define-test loop-while-expr
  (is equal
      10
      (let ((n 0))
        (khazern:loop while (< (incf n) 10))
        n)))

(define-test loop-do
  (is equal
      10
      (let ((n 0))
        (block abc
          (khazern:loop do (progn (incf n)
                             (when (= n 10)
                               (return-from abc nil)))))
        n)))

#+(or)(define-test loop-and-init
  (is equal
      '(-1 0 2 5 9)
      (let ((i -1))
        (loop for i from 1 to 5 and
                  j = i then (+ j i)
              collect j))))
