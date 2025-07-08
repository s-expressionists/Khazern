# Khazern

Khazern is a portable and extensible Common Lisp LOOP
implementation. It was originally written by Robert Strandh as part of
[SICL][]. It can be loaded intrinsically in order to replace LOOP in
an existing Lisp implementation or extrinsically to coexist with the
implementation's own LOOP.

To load Khazern intrinsically with [Quicklisp][] do the following:

```common-lisp
* (ql:quickload :khazern-intrinsic)
* (loop for i in '(1 2 3 4) when (oddp i) collect i)    
(1 3)
```

To load Khazern extrinsically do the following

```common-lisp
* (ql:quickload :khazern-extrinsic)
* (khazern-extrinsic:loop for i in '(1 2 3 4) when (oddp i) collect i)    
(1 3)
```

## Extending Khazern

Khazern supports extension via iteration paths. The documentation
regarding the this protocol has not been written yet, but examples can
be seen in the `khazern-extension` system. Khazern can support the
iteration paths provided by [loop-iteration-paths][] simply by using
either the `khazern-extension-extrinsic` or
`khazern-extension-intrinsic` systems. For example:

```common-lisp
* (ql:quickload '(:khazern-extension-extrinsic :flexi-streams))
* (kee:loop for i being the elements in #(1 2 3 4 5)
              start 1 from-end t
            do (print i))

5 
4 
3 
2 
NIL
* (with-input-from-string (stream "a 233d0 #C(1 2) (wibble)")
    (kee:loop for i being the objects in stream
              do (print i)))

A 
233.0d0 
#C(1 2) 
(WIBBLE) 
NIL
* (with-input-from-string (stream "abcd")
    (kee:loop for i being the characters in stream
              do (print i)))

#\a 
#\b 
#\c 
#\d 
NIL
* (with-input-from-string (stream "ab
c
d")
    (kee:loop for i being the lines in stream
                using (missing-newline-p j)
              do (print i)
                 (print j)))

"ab" 
NIL 
"c" 
NIL 
"d" 
T 
NIL
* (flexi-streams:with-input-from-sequence (stream #(1 2 3))
    (kee:loop for i being the bytes in stream
              do (print i)))

1 
2 
3 
NIL
```

## Replacing Builtin LOOP with Khazern

Replacing a Common Lisp's LOOP implementation with Khazern can be done
with the khazern-instrinsic system. This system is actually intended
for use as the CL implementation's original LOOP implementation.
Because LOOP is in the COMMON-LISP package ASDF logic regarding
recompilation of systems dependent on khazern-intrinsic may not be
reliable. Instead if one wants to use Khazern as the LOOP
implementation in a system it is better to use khazern-extrinsic and
then SHADOWING-IMPORT-FROM in DEFPACKAGE.

```common-lisp
;;; quux.asd


(asdf:defsystem "quux"
  :depends-on ("khazern-extrinsic")
  :components ((:file "quux")))

;;; quux.lisp

(cl:defpackage #:quux
  (:use #:common-lisp)
  (:shadowing-import-from #:khazern-extrinsic
                          #:loop
                          #:loop-finish))

(cl:in-package #:quux)

(defun wibble (s)
  (loop for i across s
        do (print i)))
```

To use the extension system one need only replace the dependency in the ASD file.

```common-lisp
;;; quux.asd


(asdf:defsystem "quux"
  :depends-on ("khazern-extension-extrinsic")
  :components ((:file "quux")))

;;; quux.lisp

(cl:defpackage #:quux
  (:use #:common-lisp)
  (:shadowing-import-from #:khazern-extension-extrinsic
                          #:loop
                          #:loop-finish))

(cl:in-package #:quux)

(defun wibble (s)
  (loop for i being the elements in s
        do (print i)))
```

[loop-iteration-paths]: https://github.com/yitzchak/loop-iteration-paths/
[Quicklisp]: https://www.quicklisp.org/beta/
[SICL]: https://github.com/robert-strandh/SICL