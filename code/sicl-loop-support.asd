;;;; Copyright (c) 2008 - 2015
;;;;
;;;;     Robert Strandh (robert.strandh@gmail.com)
;;;;
;;;; all rights reserved. 
;;;;
;;;; Permission is hereby granted to use this software for any 
;;;; purpose, including using, modifying, and redistributing it.
;;;;
;;;; The software is provided "as-is" with no warranty.  The user of
;;;; this software assumes any responsibility of the consequences. 

(cl:in-package #:asdf-user)

(defsystem :sicl-loop-support
  :depends-on (:sicl-additional-conditions
	       :acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "compile-time-conditions")
   (:file "run-time-conditions")
   (:file "condition-reporters-english")
   (:file "utilities")
   (:file "combinatory-parsing")
   (:file "parse-common")
   (:file "clause")
   (:file "expansion")
   (:file "main-clause")
   (:file "variable-clause")
   (:file "selectable-clause")
   (:file "unconditional-clause")
   (:file "accumulation-clause")
   (:file "termination-test-clause")
   (:file "var-spec")
   (:file "type-spec")
   (:file "name-clause")
   (:file "initial-clause")
   (:file "final-clause")
   (:file "with-clause")
   (:file "return-clause")
   (:file "do-clause")
   (:file "collect-clause")
   (:file "append-clause")
   (:file "nconc-clause")
   (:file "count-clause")
   (:file "sum-clause")
   (:file "maximize-clause")
   (:file "minimize-clause")
   (:file "conditional-clause")
   (:file "while-until-clauses")
   (:file "repeat-clause")
   (:file "always-clause")
   (:file "never-clause")
   (:file "thereis-clause")
   (:file "for-as-clause")
   (:file "for-as-arithmetic-clause")
   (:file "for-as-list-clause")
   (:file "for-as-equals-then-clause")
   (:file "for-as-across-clause")
   (:file "for-as-hash-clause")
   (:file "for-as-package-clause")
   (:file "analysis")
   (:file "run-time-support")))
