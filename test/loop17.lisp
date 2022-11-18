;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Nov 21 09:48:38 2002
;;;; Contains: Miscellaneous loop tests

(cl:in-package :khazern/test)

;;; Initially and finally take multiple forms,
;;; and execute them in the right order
(deftest loop.17.1
  (khazern:loop
   with x = 0
   initially (incf x 1) (incf x (+ x x))
   initially (incf x (+ x x x))
   until t
   finally (incf x 100) (incf x (+ x x))
   finally (return x))
  336)

(deftest loop.17.2
  (khazern:loop
   with x = 0
   until t
   initially (incf x 1) (incf x (+ x x))
   finally (incf x 100) (incf x (+ x x))
   initially (incf x (+ x x x))
   finally (return x))
  336)

(deftest loop.17.3
  (let ((x 0))
    (khazern:loop
     with y = (incf x 1)
     initially (incf x 2)
     until t
     finally (return (values x y))))
  3 1)

(deftest loop.17.4
  (khazern:loop
   doing (return 'a)
   finally (return 'b))
  a)

(deftest loop.17.5
  (khazern:loop
   return 'a
   finally (return 'b))
  a)

(deftest loop.17.6
  (let ((x 0))
    (tagbody
     (khazern:loop
      do (go done)
      finally (incf x))
     done)
    x)
  0)

(deftest loop.17.7
  (let ((x 0))
    (catch 'done
      (khazern:loop
       do (throw 'done nil)
       finally (incf x)))
    x)
  0)

(deftest loop.17.8
  (khazern:loop
   for x in '(1 2 3)
   collect x
   finally (return 'good))
  good)

(deftest loop.17.9
  (khazern:loop
   for x in '(1 2 3)
   append (list x)
   finally (return 'good))
  good)

(deftest loop.17.10
  (khazern:loop
   for x in '(1 2 3)
   nconc (list x)
   finally (return 'good))
  good)

(deftest loop.17.11
  (khazern:loop
   for x in '(1 2 3)
   count (> x 1)
   finally (return 'good))
  good)

(deftest loop.17.12
  (khazern:loop
   for x in '(1 2 3)
   sum x
   finally (return 'good))
  good)

(deftest loop.17.13
  (khazern:loop
   for x in '(1 2 3)
   maximize x
   finally (return 'good))
  good)

(deftest loop.17.14
  (khazern:loop
   for x in '(1 2 3)
   minimize x
   finally (return 'good))
  good)

;;; iteration clause grouping

;; (deftest loop.17.20
;;   (khazern:loop
;;    for i from 1 to 5
;;    for j = 0 then (+ j i)
;;    collect j)
;;   (0 2 5 9 14))

;; (deftest loop.17.21
;;   (khazern:loop
;;    for i from 1 to 5
;;    and j = 0 then (+ j i)
;;    collect j)
;;   (0 1 3 6 10))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.17.22
  (macrolet
   ((%m (z) z))
   (khazern:loop with x = 0
         initially (expand-in-current-env (%m (incf x)))
         until t
         finally (expand-in-current-env (%m (return x)))))
  1)
