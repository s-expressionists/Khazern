;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 17 08:47:43 2002
;;;; Contains: Tests for ALWAYS, NEVER, THEREIS

(cl:in-package :khazern/test)

;;; Tests of ALWAYS clauses

(deftest loop.12.1
  (khazern:loop for i in '(1 2 3 4) always (< i 10))
  t)

(deftest loop.12.2
  (khazern:loop for i in nil always nil)
  t)

(deftest loop.12.3
  (khazern:loop for i in '(a) always nil)
  nil)

(deftest loop.12.4
  (khazern:loop for i in '(1 2 3 4 5 6 7)
        always t
        until (> i 5))
  t)

(deftest loop.12.5
  (khazern:loop for i in '(1 2 3 4 5 6 7)
        always (< i 6)
        until (>= i 5))
  t)

(deftest loop.12.6
  (khazern:loop for x in '(a b c d e) always x)
  t)

(deftest loop.12.7
  (khazern:loop for x in '(1 2 3 4 5 6)
        always (< x 20)
        never (> x 10))
  t)

(deftest loop.12.8
  (khazern:loop for x in '(1 2 3 4 5 6)
        always (< x 20)
        never (> x 5))
  nil)

(deftest loop.12.9
  (khazern:loop for x in '(1 2 3 4 5 6)
        never (> x 5)
        always (< x 20))
  nil)

(deftest loop.12.10
  (khazern:loop for x in '(1 2 3 4 5)
        always (< x 10)
        finally (return 'good))
  good)

(deftest loop.12.11
  (khazern:loop for x in '(1 2 3 4 5)
        always (< x 3)
        finally (return 'bad))
  nil)

(deftest loop.12.12
  (khazern:loop for x in '(1 2 3 4 5 6)
        always t
        when (= x 4) do (khazern:loop-finish))
  t)

(deftest loop.12.13
  (khazern:loop for x in '(1 2 3 4 5 6)
        do (khazern:loop-finish)
        always nil)
  t)

;;; Tests of NEVER

(deftest loop.12.21
  (khazern:loop for i in '(1 2 3 4) never (> i 10))
  t)

(deftest loop.12.22
  (khazern:loop for i in nil never t)
  t)

(deftest loop.12.23
  (khazern:loop for i in '(a) never t)
  nil)

(deftest loop.12.24
  (khazern:loop for i in '(1 2 3 4 5 6 7)
        never nil
        until (> i 5))
  t)

(deftest loop.12.25
  (khazern:loop for i in '(1 2 3 4 5 6 7)
        never (>= i 6)
        until (>= i 5))
  t)

(deftest loop.12.26
  (khazern:loop for x in '(a b c d e) never (not x))
  t)

(deftest loop.12.30
  (khazern:loop for x in '(1 2 3 4 5)
        never (>= x 10)
        finally (return 'good))
  good)

(deftest loop.12.31
  (khazern:loop for x in '(1 2 3 4 5)
        never (>= x 3)
        finally (return 'bad))
  nil)

(deftest loop.12.32
  (khazern:loop for x in '(1 2 3 4 5 6)
        never nil
        when (= x 4) do (khazern:loop-finish))
  t)

(deftest loop.12.33
  (khazern:loop for x in '(1 2 3 4 5 6)
        do (khazern:loop-finish)
        never t)
  t)

;;; Tests of THEREIS

(deftest loop.12.41
  (khazern:loop for x in '(1 2 3 4 5)
        thereis (and (eql x 3) 'good))
  good)

(deftest loop.12.42
  (khazern:loop for x in '(nil nil a nil nil)
        thereis x)
  a)

(deftest loop.12.43
  (khazern:loop for x in '(1 2 3 4 5)
        thereis (eql x 4)
        when (eql x 2) do (khazern:loop-finish))
  nil)

;;; Error cases

(deftest loop.12.error.50
  (signals-error
   (khazern:loop for i from 1 to 10
         collect i
         always (< i 20))
   program-error)
  t)

(deftest loop.12.error.50a
  (signals-error
   (khazern:loop for i from 1 to 10
         always (< i 20)
         collect i)
   program-error)
  t)

(deftest loop.12.error.51
  (signals-error
   (khazern:loop for i from 1 to 10
         collect i
         never (> i 20))
   program-error)
  t)

(deftest loop.12.error.51a
  (signals-error
   (khazern:loop for i from 1 to 10
         never (> i 20)
         collect i)
   program-error)
  t)

(deftest loop.12.error.52
  (signals-error
   (khazern:loop for i from 1 to 10
         collect i
         thereis (> i 20))
   program-error)
  t)

(deftest loop.12.error.52a
  (signals-error
   (khazern:loop for i from 1 to 10
         thereis (> i 20)
         collect i)
   program-error)
  t)

;;; Non-error cases

(deftest loop.12.53
  (khazern:loop for i from 1 to 10
        collect i into foo
        always (< i 20))
  t)

(deftest loop.12.53a
  (khazern:loop for i from 1 to 10
        always (< i 20)
        collect i into foo)
  t)

(deftest loop.12.54
  (khazern:loop for i from 1 to 10
        collect i into foo
        never (> i 20))
  t)

(deftest loop.12.54a
  (khazern:loop for i from 1 to 10
        never (> i 20)
        collect i into foo)
  t)

(deftest loop.12.55
  (khazern:loop for i from 1 to 10
        collect i into foo
        thereis i)
  1)

(deftest loop.12.55a
  (khazern:loop for i from 1 to 10
        thereis i
        collect i into foo)
  1)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.12.56
  (macrolet
   ((%m (z) z))
   (khazern:loop for i in '(1 2 3 4) always (expand-in-current-env (%m (< i 10)))))
  t)

(deftest loop.12.57
  (macrolet
   ((%m (z) z))
   (khazern:loop for i in '(1 2 3 4) always (expand-in-current-env (%m t))))
  t)

(deftest loop.12.58
  (macrolet
   ((%m (z) z))
   (khazern:loop for i in '(1 2 3 4) never (expand-in-current-env (%m (>= i 10)))))
  t)

(deftest loop.12.59
  (macrolet
   ((%m (z) z))
   (khazern:loop for i in '(1 2 3 4) never (expand-in-current-env (%m t))))
  nil)

(deftest loop.12.60
  (macrolet
   ((%m (z) z))
   (khazern:loop for i in '(1 2 3 4)
         thereis (expand-in-current-env (%m (and (>= i 2) (+ i 1))))))
  3)
