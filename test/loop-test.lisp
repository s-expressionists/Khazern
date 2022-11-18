(cl:in-package #:khazern/test)

(defmacro deftest (name form &rest results)
  (if (and (listp form)
           (eql 'signals-error (car form)))
      `(parachute:define-test ,name
         :compile-at :execute
         (parachute:fail ,@(cdr form)))
      `(parachute:define-test ,name
         :compile-at :execute
         (parachute:is equal
           (values ,@(mapcar (lambda (result)      
                               (list 'quote result))
                             results))
           ,form))))

(defmacro expand-in-current-env (macro-form &environment env)
  (macroexpand macro-form env))

(deftest loop-finish-in-simple-loop
    (khazern:loop do (khazern:loop (khazern:loop-finish)))
  nil)

(deftest loop-until-t
    (khazern:loop until t)
  nil)

(deftest loop-while-nil
    (khazern:loop while nil)
  nil)

(deftest loop-until-expr
    (let ((n 0))
      (khazern:loop until (= (incf n) 10))
      n)
  10)

(deftest loop-while-expr
    (let ((n 0))
      (khazern:loop while (< (incf n) 10))
      n)
  10)

(deftest loop-do
    (let ((n 0))
      (block abc
        (khazern:loop do (progn (incf n)
                        (when (= n 10)
                          (return-from abc nil)))))
      n)
  10)

(defun loop-until-do ()
  (assert (equal (let ((n 0))
                   (khazern:loop until (= n 10)
                                 do (incf n))
                   n)
                 10)))

(defun loop-repeat-do ()
  (assert (equal (let ((n 0))
                   (khazern:loop repeat 5
                                 do (incf n 2))
                   n)
                 10)))

(defun loop-with-repeat-do ()
  (assert (equal (let ((n 0))
                   (khazern:loop with step = 2
                         repeat 5
                         do (incf n step))
                   n)
                 10)))

(defun loop-initially-repeat-do ()
  (assert (equal (let ((n 0))
                   (khazern:loop initially (incf n 10)
                         repeat 2
                         do (setf n (* n 2)))
                   n)
                 40)))

(defun loop-repeat-do-finally ()
  (assert (equal (let ((n 0))
                   (khazern:loop repeat 2
                         do (incf n 10)
                         finally (setf n (* n 2)))
                   n)
                 40)))

;; (defun loop-with-repeat-do-collect-finally ()
;;   (assert (equal (let ((result nil))
;;                 (khazern:loop with n = 0
;;                       repeat 4
;;                       do (incf n)
;;                       collect n into foo
;;                       finally (setf result foo))
;;                 result)
;;               '(1 2 3 4))))

;; (defun loop-repeat-sum-finally ()
;;   (assert (equal (let ((result nil))
;;                 (khazern:loop repeat 5
;;                       sum 2 into foo
;;                       finally (setf result foo))
;;                 result)
;;               10)))

