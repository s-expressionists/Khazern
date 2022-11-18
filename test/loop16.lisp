;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 21 09:46:27 2002
;;;; Contains: Tests that uninterned symbols can be loop keywords

(cl:in-package :khazern/test)


(deftest loop.16.30
  (khazern:loop #:for i #:from 1 #:to 10 #:collect i)
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.16.31
  (khazern:loop #:for i #:upfrom 1 #:below 10 #:by 2 #:collect i)
  (1 3 5 7 9))

(deftest loop.16.32
  (khazern:loop #:with x = 1 #:and y = 2 #:return (values x y))
  1 2)

(deftest loop.16.33
  (khazern:loop #:named foo #:doing (return-from foo 1))
  1)

(deftest loop.16.34
  (let ((x 0))
    (khazern:loop
     #:initially (setq x 2)
     #:until t
     #:finally (return x)))
  2)

(deftest loop.16.35
  (khazern:loop #:for x #:in '(a b c) #:collecting x)
  (a b c))

(deftest loop.16.36
  (khazern:loop #:for x #:in '(a b c) #:append (list x))
  (a b c))

(deftest loop.16.37
  (khazern:loop #:for x #:in '(a b c) #:appending (list x))
  (a b c))

(deftest loop.16.38
  (khazern:loop #:for x #:in '(a b c) #:nconc (list x))
  (a b c))

(deftest loop.16.39
  (khazern:loop #:for x #:in '(a b c) #:nconcing (list x))
  (a b c))

(deftest loop.16.40
  (khazern:loop #:for x #:in '(1 2 3) #:count x)
  3)

(deftest loop.16.41
  (khazern:loop #:for x #:in '(1 2 3) #:counting x)
  3)

(deftest loop.16.42
  (khazern:loop #:for x #:in '(1 2 3) #:sum x)
  6)

(deftest loop.16.43
  (khazern:loop #:for x #:in '(1 2 3) #:summing x)
  6)

(deftest loop.16.44
  (khazern:loop #:for x #:in '(10 20 30) #:maximize x)
  30)

(deftest loop.16.45
  (khazern:loop #:for x #:in '(10 20 30) #:maximizing x)
  30)

(deftest loop.16.46
  (khazern:loop #:for x #:in '(10 20 30) #:minimize x)
  10)

(deftest loop.16.47
  (khazern:loop #:for x #:in '(10 20 30) #:minimizing x)
  10)

(deftest loop.16.48
  (khazern:loop #:for x #:in '(1 2 3 4) #:sum x #:into foo #:of-type fixnum
        #:finally (return foo))
  10)

(deftest loop.16.49
  (khazern:loop #:for x #:upfrom 1 #:to 10
        #:if (evenp x) #:sum x #:into foo
        #:else #:sum x #:into bar
        #:end
        #:finally (return (values foo bar)))
  30 25)

(deftest loop.16.50
  (khazern:loop #:for x #:downfrom 10 #:above 0
        #:when (evenp x) #:sum x #:into foo
        #:else #:sum x #:into bar
        #:end
        #:finally (return (values foo bar)))
  30 25)

(deftest loop.16.51
  (khazern:loop #:for x #:in '(a b nil c d nil)
        #:unless x #:count t)
  2)

(deftest loop.16.52
  (khazern:loop #:for x #:in '(a b nil c d nil)
        #:unless x #:collect x #:into bar #:and #:count t #:into foo
        #:end
        finally (return (values bar foo)))
  (nil nil)
  2)

(deftest loop.16.53
  (khazern:loop #:for x #:in '(nil nil a b nil c nil)
        #:collect x
        #:until x)
  (nil nil a))

(deftest loop.16.54
  (khazern:loop #:for x #:in '(a b nil c nil)
        #:while x #:collect x)
  (a b))

(deftest loop.16.55
  (khazern:loop #:for x #:in '(nil nil a b nil c nil)
        #:thereis x)
  a)

(deftest loop.16.56
  (khazern:loop #:for x #:in '(nil nil a b nil c nil)
        #:never x)
  nil)

(deftest loop.16.57
  (khazern:loop #:for x #:in '(a b c d e)
        #:always x)
  t)

(deftest loop.16.58
  (khazern:loop #:as x #:in '(a b c) #:count t)
  3)

(deftest loop.16.59
  (khazern:loop #:for i #:from 10 #:downto 5 #:collect i)
  (10 9 8 7 6 5))

(deftest loop.16.60
  (khazern:loop #:for i #:from 0 #:upto 5 #:collect i)
  (0 1 2 3 4 5))

(deftest loop.16.61
  (khazern:loop #:for x #:on '(a b c) #:collecting (car x))
  (a b c))

(deftest loop.16.62
  (khazern:loop #:for x = '(a b c) #:then (cdr x)
        #:while x
        #:collect (car x))
  (a b c))

(deftest loop.16.63
  (khazern:loop #:for x #:across #(a b c) #:collect x)
  (a b c))

(deftest loop.16.64
  (khazern:loop #:for x #:being #:the #:hash-keys #:of (make-hash-table)
        #:count t)
  0)

(deftest loop.16.65
  (khazern:loop #:for x #:being #:each #:hash-key #:in (make-hash-table)
        #:count t)
  0)

(deftest loop.16.66
  (khazern:loop #:for x #:being #:each #:hash-value #:of (make-hash-table)
        #:count t)
  0)

(deftest loop.16.67
  (khazern:loop #:for x #:being #:the #:hash-values #:in (make-hash-table)
        #:count t)
  0)

(deftest loop.16.68
  (khazern:loop #:for x #:being #:the #:hash-values #:in (make-hash-table)
        #:using (#:hash-key k)
        #:count t)
  0)

(deftest loop.16.69
  (khazern:loop #:for x #:being #:the #:hash-keys #:in (make-hash-table)
        #:using (#:hash-value v)
        #:count t)
  0)

(deftest loop.16.70
  (let ()
    (ignore-errors (delete-package "LOOP.16.PACKAGE"))
    (let ((p (make-package "LOOP.16.PACKAGE" :use nil)))
      (khazern:loop #:for x #:being #:the #:symbols #:of p #:count t)))
  0)

(deftest loop.16.71
  (let ()
    (ignore-errors (delete-package "LOOP.16.PACKAGE"))
    (let ((p (make-package "LOOP.16.PACKAGE" :use nil)))
      (khazern:loop #:for x #:being #:each #:symbol #:of p #:count t)))
  0)

(deftest loop.16.72
  (let ()
    (ignore-errors (delete-package "LOOP.16.PACKAGE"))
    (let ((p (make-package "LOOP.16.PACKAGE" :use nil)))
      (khazern:loop #:for x #:being #:the #:external-symbols #:of p #:count t)))
  0)

(deftest loop.16.73
  (let ()
    (ignore-errors (delete-package "LOOP.16.PACKAGE"))
    (let ((p (make-package "LOOP.16.PACKAGE" :use nil)))
      (khazern:loop #:for x #:being #:each #:external-symbol #:of p #:count t)))
  0)

(deftest loop.16.74
  (let ()
    (ignore-errors (delete-package "LOOP.16.PACKAGE"))
    (let ((p (make-package "LOOP.16.PACKAGE" :use nil)))
      (khazern:loop #:for x #:being #:the #:present-symbols #:of p #:count t)))
  0)

(deftest loop.16.75
  (let ()
    (ignore-errors (delete-package "LOOP.16.PACKAGE"))
    (let ((p (make-package "LOOP.16.PACKAGE" :use nil)))
      (khazern:loop #:for x #:being #:each #:present-symbol #:of p #:count t)))
  0)
