;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 17 12:37:45 2002
;;;; Contains: Tests of DO, DOING, RETURN in LOOP.  Tests of NAMED loops

(cl:in-package :khazern/test)

(deftest loop.13.1
  (khazern:loop do (return 10))
  10)

(deftest loop.13.2
  (khazern:loop doing (return 10))
  10)

(deftest loop.13.3
  (khazern:loop for i from 0 below 100 by 7
        when (> i 50) return i)
  56)

(deftest loop.13.4
  (let ((x 0))
    (khazern:loop do
          (incf x)
          (when (= x 10) (return x))))
  10)

(deftest loop.13.5
  (khazern:loop return 'a)
  a)

(deftest loop.13.6
  (khazern:loop return (values)))

(deftest loop.13.7
  (khazern:loop return (values 1 2))
  1 2)

(deftest loop.13.8
  (let* ((limit (min 1000 (1- (min call-arguments-limit
                                   multiple-values-limit))))
         (vals (make-list limit :initial-element :a))
         (vals2 (multiple-value-list (eval `(khazern:loop return (values ,@vals))))))
    (equal vals vals2))
  t)

(deftest loop.13.9
  (khazern:loop named foo return 'a)
  a)

(deftest loop.13.10
  (block nil
    (return (khazern:loop named foo return :good))
    :bad)
  :good)

(deftest loop.13.11
  (block nil
    (khazern:loop named foo do (return :good))
    :bad)
  :good)

(deftest loop.13.12
  (khazern:loop named foo with a = (return-from foo :good) return :bad)
  :good)

(deftest loop.13.13
  (khazern:loop named foo
        with b = 1
        and a = (return-from foo :good) return :bad)
  :good)

(deftest loop.13.14
  (khazern:loop named foo
        for a = (return-from foo :good) return :bad)
  :good)

(deftest loop.13.15
  (khazern:loop named foo for a in (return-from foo :good))
  :good)

(deftest loop.13.16
  (khazern:loop named foo for a from (return-from foo :good) return :bad)
  :good)

(deftest loop.13.17
  (khazern:loop named foo for a on (return-from foo :good) return :bad)
  :good)

(deftest loop.13.18
  (khazern:loop named foo for a across (return-from foo :good) return :bad)
  :good)

(deftest loop.13.19
  (khazern:loop named foo for a being the hash-keys of (return-from foo :good)
        return :bad)
  :good)

(deftest loop.13.20
  (khazern:loop named foo for a being the symbols of (return-from foo :good)
        return :bad)
  :good)

(deftest loop.13.21
  (khazern:loop named foo repeat (return-from foo :good) return :bad)
  :good)

(deftest loop.13.22
  (khazern:loop named foo for i from 0 to (return-from foo :good) return :bad)
  :good)

(deftest loop.13.23
  (khazern:loop named foo for i from 0 to 10 by (return-from foo :good) return :bad)
  :good)

(deftest loop.13.24
  (khazern:loop named foo for i from 10 downto (return-from foo :good) return :bad)
  :good)

(deftest loop.13.25
  (khazern:loop named foo for i from 10 above (return-from foo :good) return :bad)
  :good)

(deftest loop.13.26
  (khazern:loop named foo for i from 10 below (return-from foo :good) return :bad)
  :good)

(deftest loop.13.27
  (khazern:loop named foo for i in '(a b c) by (return-from foo :good) return :bad)
  :good)

(deftest loop.13.28
  (khazern:loop named foo for i on '(a b c) by (return-from foo :good) return :bad)
  :good)

(deftest loop.13.29
  (khazern:loop named foo for i = 1 then (return-from foo :good))
  :good)

(deftest loop.13.30
  (khazern:loop named foo for x in '(a b c) collect (return-from foo :good))
  :good)

(deftest loop.13.31
  (khazern:loop named foo for x in '(a b c) append (return-from foo :good))
  :good)

(deftest loop.13.32
  (khazern:loop named foo for x in '(a b c) nconc (return-from foo :good))
  :good)

(deftest loop.13.33
  (khazern:loop named foo for x in '(a b c) count (return-from foo :good))
  :good)

(deftest loop.13.34
  (khazern:loop named foo for x in '(a b c) sum (return-from foo :good))
  :good)

(deftest loop.13.35
  (khazern:loop named foo for x in '(a b c) maximize (return-from foo :good))
  :good)

(deftest loop.13.36
  (khazern:loop named foo for x in '(a b c) minimize (return-from foo :good))
  :good)

(deftest loop.13.37
  (khazern:loop named foo for x in '(a b c) thereis (return-from foo :good))
  :good)

(deftest loop.13.38
  (khazern:loop named foo for x in '(a b c) always (return-from foo :good))
  :good)

(deftest loop.13.39
  (khazern:loop named foo for x in '(a b c) never (return-from foo :good))
  :good)

(deftest loop.13.40
  (khazern:loop named foo for x in '(a b c) until (return-from foo :good))
  :good)

(deftest loop.13.41
  (khazern:loop named foo for x in '(a b c) while (return-from foo :good))
  :good)

(deftest loop.13.42
  (khazern:loop named foo for x in '(a b c) when (return-from foo :good) return :bad)
  :good)

(deftest loop.13.43
  (khazern:loop named foo for x in '(a b c) unless (return-from foo :good) return :bad)
  :good)

(deftest loop.13.44
  (khazern:loop named foo for x in '(a b c) if (return-from foo :good) return :bad)
  :good)

(deftest loop.13.45
  (khazern:loop named foo for x in '(a b c) return (return-from foo :good))
  :good)

(deftest loop.13.46
  (khazern:loop named foo initially (return-from foo :good) return :bad)
  :good)

(deftest loop.13.47
  (khazern:loop named foo do (khazern:loop-finish) finally (return-from foo :good))
  :good)


(deftest loop.13.52
  (block nil
    (khazern:loop named foo with a = (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.53
  (block nil
    (khazern:loop named foo
          with b = 1
          and a = (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.54
  (block nil
    (khazern:loop named foo
          for a = (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.55
  (block nil
    (khazern:loop named foo for a in (return :good))
    :bad)
  :good)

(deftest loop.13.56
  (block nil
    (khazern:loop named foo for a from (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.57
  (block nil
    (khazern:loop named foo for a on (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.58
  (block nil
    (khazern:loop named foo for a across (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.59
  (block nil
    (khazern:loop named foo for a being the hash-keys of (return :good)
          return :bad)
    :bad)
  :good)

(deftest loop.13.60
  (block nil
    (khazern:loop named foo for a being the symbols of (return :good)
          return :bad)
    :bad)
  :good)

(deftest loop.13.61
  (block nil
    (khazern:loop named foo repeat (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.62
  (block nil
    (khazern:loop named foo for i from 0 to (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.63
  (block nil
    (khazern:loop named foo for i from 0 to 10 by (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.64
  (block nil
    (khazern:loop named foo for i from 10 downto (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.65
  (block nil
    (khazern:loop named foo for i from 10 above (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.66
  (block nil
    (khazern:loop named foo for i from 10 below (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.67
  (block nil
    (khazern:loop named foo for i in '(a b c) by (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.68
  (block nil
    (khazern:loop named foo for i on '(a b c) by (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.69
  (block nil
    (khazern:loop named foo for i = 1 then (return :good))
    :bad)
  :good)

(deftest loop.13.70
  (block nil
    (khazern:loop named foo for x in '(a b c) collect (return :good))
    :bad)
  :good)

(deftest loop.13.71
  (block nil
    (khazern:loop named foo for x in '(a b c) append (return :good))
    :bad)
  :good)

(deftest loop.13.72
  (block nil
    (khazern:loop named foo for x in '(a b c) nconc (return :good))
    :bad)
  :good)

(deftest loop.13.73
  (block nil
    (khazern:loop named foo for x in '(a b c) count (return :good))
    :bad)
  :good)

(deftest loop.13.74
  (block nil
    (khazern:loop named foo for x in '(a b c) sum (return :good))
    :bad)
  :good)

(deftest loop.13.75
  (block nil
    (khazern:loop named foo for x in '(a b c) maximize (return :good))
    :bad)
  :good)

(deftest loop.13.76
  (block nil
    (khazern:loop named foo for x in '(a b c) minimize (return :good))
    :bad)
  :good)

(deftest loop.13.77
  (block nil
    (khazern:loop named foo for x in '(a b c) thereis (return :good))
    :bad)
  :good)

(deftest loop.13.78
  (block nil
    (khazern:loop named foo for x in '(a b c) always (return :good))
    :bad)
  :good)

(deftest loop.13.79
  (block nil
    (khazern:loop named foo for x in '(a b c) never (return :good))
    :bad)
  :good)

(deftest loop.13.80
  (block nil
    (khazern:loop named foo for x in '(a b c) until (return :good))
    :bad)
  :good)

(deftest loop.13.81
  (block nil
    (khazern:loop named foo for x in '(a b c) while (return :good))
    :bad)
  :good)

(deftest loop.13.82
  (block nil
    (khazern:loop named foo for x in '(a b c) when (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.83
  (block nil
    (khazern:loop named foo for x in '(a b c) unless (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.84
  (block nil
    (khazern:loop named foo for x in '(a b c) if (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.85
  (block nil
    (khazern:loop named foo for x in '(a b c) return (return :good))
    :bad)
  :good)

(deftest loop.13.86
  (block nil
    (khazern:loop named foo initially (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.87
  (block nil
    (khazern:loop named foo do (khazern:loop-finish) finally (return :good))
    :bad)
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.13.88
  (macrolet
   ((%m (z) z))
   (khazern:loop do (expand-in-current-env (%m (return 10)))))
  10)

(deftest loop.13.89
  (macrolet
   ((%m (z) z))
   (khazern:loop for i from 0 below 100 by 7
         when (> i 50) return (expand-in-current-env (%m i))))
  56)

(deftest loop.13.90
  (macrolet
   ((%m (z) z))
   (khazern:loop return (expand-in-current-env (%m 'a))))
  a)
