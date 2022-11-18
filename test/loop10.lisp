;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 16 09:07:02 2002
;;;; Contains: Tests of LOOP numeric value accumulation clauses

(cl:in-package :khazern/test)

;; Tests of COUNT, COUNTING

(deftest loop.10.1
  (khazern:loop for x from 1 to 10 count (< x 5))
  4)

(deftest loop.10.2
  (khazern:loop for x from 1 to 10 counting (< x 7))
  6)

(deftest loop.10.3
  (khazern:loop for x from 1 to 10 count (< x 5) fixnum)
  4)

(deftest loop.10.4
  (khazern:loop for x from 1 to 10 count (< x 5) of-type integer)
  4)

(deftest loop.10.5
  (let (z)
    (values
     (khazern:loop for x from 1 to 10 count (< x 5) into foo
           finally (setq z foo))
     z))
  nil
  4)

(deftest loop.10.6
  (let (z)
    (values
     (khazern:loop for x from 1 to 10 count (< x 5) into foo fixnum
           finally (setq z foo))
     z))
  nil
  4)

(deftest loop.10.7
  (let (z)
    (values
     (khazern:loop for x from 1 to 10 count (< x 5) into foo of-type (integer 0 100)
           finally (setq z foo))
     z))
  nil
  4)

(deftest loop.10.8
  (let (z)
    (values
     (khazern:loop for x from 1 to 10 count (< x 5) into foo float
           finally (setq z foo))
     z))
  nil
  4.0)

(deftest loop.10.9
  (signals-error
   (khazern:loop with foo = 10
         for x in '(a b c) count x into foo
         finally (return foo))
   program-error)
  t)

(deftest loop.10.10
  (signals-error
   (khazern:loop with foo = 10
         for x in '(a b c) counting x into foo
         finally (return foo))
   program-error)
  t)

(declaim (special *loop-count-var*))

(deftest loop.10.11
  (let ((*loop-count-var* 100))
    (values
     (khazern:loop for x in '(a b c d) count x into *loop-count-var*
           finally (return *loop-count-var*))
     *loop-count-var*))
  4 100)

(deftest loop.10.12
  (khazern:loop for x in '(a b nil d nil e)
        count x into foo
        collect foo)
  (1 2 2 3 3 4))

(deftest loop.10.13
  (khazern:loop for x in '(a b nil d nil e)
        counting x into foo
        collect foo)
  (1 2 2 3 3 4))

(deftest loop.10.14
  (khazern:loop for x in '(a b c) count (return 10))
  10)


;;; Tests of MAXIMIZE, MAXIMIZING

(deftest loop.10.20
  (khazern:loop for x in '(1 4 10 5 7 9) maximize x)
  10)

(deftest loop.10.21
  (khazern:loop for x in '(1 4 10 5 7 9) maximizing x)
  10)

(deftest loop.10.22
  (khazern:loop for x in '(1000000000000) maximizing x)
  1000000000000)

(deftest loop.10.23
  (khazern:loop for x in '(-1000000000000) maximize x)
  -1000000000000)

(deftest loop.10.24
  (khazern:loop for x in '(1.0 2.0 3.0 -1.0) maximize x)
  3.0)

(deftest loop.10.25
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x fixnum)
  24)

(deftest loop.10.26
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x of-type integer)
  24)

(deftest loop.10.27
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x of-type rational)
  24)

(deftest loop.10.28
  (khazern:loop for x in '(1 4 10 5 7 9) maximize x into foo finally (return foo))
  10)

(deftest loop.10.29
  (let (z)
    (values
     (khazern:loop for x in '(1 4 10 5 7 9) maximize x into foo finally (setq z foo))
     z))
  nil
  10)

(deftest loop.10.30
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) maximize x of-type real)
  24)

(deftest loop.10.31
  (khazern:loop for x in '(0.08 0.20 0.05 0.03 0.24 0.01 0.19 0.04 0.20 0.03) maximize x of-type float)
  0.24)

(deftest loop.10.32
  (khazern:loop for x in '(-1/8 -1/20 -1/5 -1/3 -1/24 -1/1 -1/19 -1/4 -1/20 -1/3) maximize x of-type rational)
  -1/24)

(deftest loop.10.33
  (khazern:loop for x in '(1 4 10 5 7 9) maximize x into foo fixnum finally (return foo))
  10)

(deftest loop.10.34
  (khazern:loop for x in '(1 4 10 5 7 9) maximize x into foo of-type integer finally (return foo))
  10)

(deftest loop.10.35
  (let ((foo 20))
    (values
     (khazern:loop for x in '(3 5 8 3 7) maximize x into foo finally (return foo))
     foo))
  8 20)

(declaim (special *loop-max-var*))

(deftest loop.10.36
  (let ((*loop-max-var* 100))
    (values
     (khazern:loop for x in '(1 10 4 8) maximize x into *loop-max-var*
           finally (return *loop-max-var*))
     *loop-max-var*))
  10 100)

(deftest loop.10.37
  (signals-error
   (khazern:loop with foo = 100
         for i from 1 to 10 maximize i into foo
         finally (return foo))
   program-error)
  t)

(deftest loop.10.38
  (signals-error
   (khazern:loop with foo = 100
         for i from 1 to 10 maximizing i into foo
         finally (return foo))
   program-error)
  t)


(deftest loop.10.39
  (khazern:loop for x in '(1 2 3) maximize (return 10))
  10)

;;; Tests of MINIMIZE, MINIMIZING

(deftest loop.10.40
  (khazern:loop for x in '(4 10 1 5 7 9) minimize x)
  1)

(deftest loop.10.41
  (khazern:loop for x in '(4 10 5 7 1 9) minimizing x)
  1)

(deftest loop.10.42
  (khazern:loop for x in '(1000000000000) minimizing x)
  1000000000000)

(deftest loop.10.43
  (khazern:loop for x in '(-1000000000000) minimize x)
  -1000000000000)

(deftest loop.10.44
  (khazern:loop for x in '(1.0 2.0 -1.0 3.0) minimize x)
  -1.0)

(deftest loop.10.45
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x fixnum)
  1)

(deftest loop.10.46
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x of-type integer)
  1)

(deftest loop.10.47
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x of-type rational)
  1)

(deftest loop.10.48
  (khazern:loop for x in '(1 4 10 5 7 9) minimize x into foo finally (return foo))
  1)

(deftest loop.10.49
  (let (z)
    (values
     (khazern:loop for x in '(4 1 10 1 5 7 9) minimize x into foo finally (setq z foo))
     z))
  nil
  1)

(deftest loop.10.50
  (khazern:loop for x in '(8 20 5 3 24 1 19 4 20 3) minimize x of-type real)
  1)

(deftest loop.10.51
  (khazern:loop for x in '(0.08 0.40 0.05 0.03 0.44 0.01 0.19 0.04 0.40 0.03) minimize x of-type float)
  0.01)

(deftest loop.10.52
  (khazern:loop for x in '(-1/8 -1/20 -1/5 -1/3 -1/24 -1/1 -1/19 -1/4 -1/20 -1/3) minimize x of-type rational)
  -1/1)

(deftest loop.10.53
  (khazern:loop for x in '(4 10 5 1 7 9) minimize x into foo fixnum finally (return foo))
  1)

(deftest loop.10.54
  (khazern:loop for x in '(1 4 10 5 7 9) minimize x into foo of-type integer finally (return foo))
  1)

(deftest loop.10.55
  (let ((foo 20))
    (values
     (khazern:loop for x in '(4 5 8 3 7) minimize x into foo finally (return foo))
     foo))
  3 20)

(declaim (special *loop-min-var*))

(deftest loop.10.56
  (let ((*loop-min-var* 100))
    (values
     (khazern:loop for x in '(10 4 8) minimize x into *loop-min-var*
           finally (return *loop-min-var*))
     *loop-min-var*))
  4 100)

(deftest loop.10.57
  (signals-error
   (khazern:loop with foo = 100
         for i from 1 to 10 minimize i into foo
         finally (return foo))
   program-error)
  t)

(deftest loop.10.58
  (signals-error
   (khazern:loop with foo = 100
         for i from 1 to 10 minimizing i into foo
         finally (return foo))
   program-error)
  t)

(deftest loop.10.58a
  (khazern:loop for x in '(1 2 3) minimize (return 10))
  10)

;;; Tests combining MINIMIZE, MAXIMIZE

(deftest loop.10.59
  (khazern:loop for i from 1 to 10
        minimize i
        maximize (- i))
  1)

(deftest loop.10.60
  (khazern:loop for i from 1 to 10
        maximize (- i)
        minimize i)
  -1)

(deftest loop.10.61
  (khazern:loop for i from 5 downto 1
        maximize i
        minimize (- i))
  -1)

;;; Tests for SUM, SUMMING

(deftest loop.10.70
  (khazern:loop for i from 1 to 4 sum i)
  10)

(deftest loop.10.71
  (khazern:loop for i from 1 to 4 summing i)
  10)

(deftest loop.10.72
  (khazern:loop for i from 1 to 4 sum (float i))
  10.0)

(deftest loop.10.73
  (khazern:loop for i from 1 to 4 sum (complex i i))
  #c(10 10))

(deftest loop.10.74
  (khazern:loop for i from 1 to 4 sum i fixnum)
  10)

(deftest loop.10.75
  (khazern:loop for i from 1 to 4 sum i of-type integer)
  10)

(deftest loop.10.76
  (khazern:loop for i from 1 to 4 sum i of-type rational)
  10)

(deftest loop.10.77
  (khazern:loop for i from 1 to 4 sum (float i) float)
  10.0)

(deftest loop.10.78
  (khazern:loop for i from 1 to 4 sum i of-type number)
  10)

(deftest loop.10.79
  (khazern:loop for i from 1 to 4 sum i into foo finally (return foo))
  10)

(deftest loop.10.80
  (khazern:loop for i from 1 to 4 sum i into foo fixnum finally (return foo))
  10)

(deftest loop.10.81
  (let (z)
    (values
     (khazern:loop for i from 1 to 4 sum i into foo of-type (integer 0 10)
           finally (setq z foo))
     z))
  nil
  10)

(deftest loop.10.82
  (khazern:loop for i from 1 to 4
        sum i fixnum
        count t)
  14)

(deftest loop.10.83
  (khazern:loop for i from 1 to 4
        sum i fixnum
        count t fixnum)
  14)

(deftest loop.10.84
  (let ((foo 100))
    (values
     (khazern:loop for i from 1 to 4 sum i into foo of-type integer
           finally (return foo))
     foo))
  10 100)

(deftest loop.10.85
  (signals-error
   (khazern:loop with foo = 100
         for i from 1 to 4 sum i into foo
         finally (return foo))
   program-error)
  t)

(deftest loop.10.86
  (signals-error
   (khazern:loop with foo = 100
         for i from 1 to 4 summing i into foo
         finally (return foo))
   program-error)
  t)

(deftest loop.10.87
  (khazern:loop for i from 1 to 4
        sum (complex i (1+ i)) of-type complex)
  #c(10 14))

(deftest loop.10.88
  (khazern:loop for i from 1 to 4
        sum (/ i 17) of-type rational)
  10/17)

(deftest loop.10.89
  (khazern:loop for i from 1 to 4 summing (/ i 17))
  10/17)

(deftest loop.10.90
  (khazern:loop for i from 1 to 4
        sum i into foo
        sum (1+ i) into bar
        finally (return (values foo bar)))
  10 14)

(deftest loop.10.91
  (khazern:loop for i from 1 to 4
        sum i into foo fixnum
        sum (float (1+ i)) into bar float
        finally (return (values foo bar)))
  10 14.0)

(deftest loop.10.92
  (khazern:loop for i from 1 to 4 sum (return 100))
  100)

(deftest loop.10.93
  (khazern:loop for i from 1 to 4 summing (return 100))
  100)

(deftest loop.10.94
  (khazern:loop for i in nil sum i of-type integer)
  0)

(deftest loop.10.95
  (khazern:loop for i in nil sum i of-type fixnum)
  0)

(deftest loop.10.96
  (khazern:loop for i in nil sum i of-type bit)
  0)

(deftest loop.10.97
  (khazern:loop for i in nil sum i of-type (integer 0 100))
  0)

(deftest loop.10.98
  (khazern:loop for i in nil sum i of-type (integer -100 0))
  0)

(deftest loop.10.99
  (khazern:loop for i in nil sum i of-type (integer -100 100))
  0)

(deftest loop.10.100
  (khazern:loop for i in nil sum i of-type (and integer (real -100.0 100.0)))
  0)

(deftest loop.10.101
  (khazern:loop for i in nil sum i of-type short-float)
  0.0s0)

(deftest loop.10.102
  (khazern:loop for i in nil sum i of-type single-float)
  0.0f0)

(deftest loop.10.103
  (khazern:loop for i in nil sum i of-type double-float)
  0.0d0)

(deftest loop.10.104
  (khazern:loop for i in nil sum i of-type long-float)
  0.0l0)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.10.105
  (macrolet
   ((%m (z) z))
   (khazern:loop for x from 1 to 10 count (expand-in-current-env (%m (< x 5)))))
  4)

(deftest loop.10.106
  (macrolet
   ((%m (z) z))
   (khazern:loop for x from 1 to 10 counting (expand-in-current-env (%m t))))
  10)

(deftest loop.10.107
  (macrolet
   ((%m (z) z))
   (khazern:loop for x from 1 to 10 count (expand-in-current-env (%m nil))))
  0)

(deftest loop.10.108
  (macrolet
   ((%m (z) z))
   (khazern:loop for x in '(1 4 10 5 7 9) maximize (expand-in-current-env (%m x))))
  10)

(deftest loop.10.109
  (macrolet
   ((%m (z) z))
   (khazern:loop for x in '(1 4 10 5 7 9) maximizing (expand-in-current-env (%m 17))))
  17)

(deftest loop.10.110
  (macrolet
   ((%m (z) z))
   (khazern:loop for x in '(5 4 10 1 7 9) minimize (expand-in-current-env (%m x))))
  1)

(deftest loop.10.111
  (macrolet
   ((%m (z) z))
   (khazern:loop for x in '(5 4 10 1 7 9) minimizing (expand-in-current-env (%m 3))))
  3)

(deftest loop.10.112
  (macrolet
   ((%m (z) z))
   (khazern:loop for x in '(1 4 10 5 7 9) sum (expand-in-current-env (%m x))))
  36)

(deftest loop.10.113
  (macrolet
   ((%m (z) z))
   (khazern:loop for x in '(1 4 10 5 7 9) summing (expand-in-current-env (%m 2))))
  12)
