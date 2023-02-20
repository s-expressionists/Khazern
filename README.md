# Khazern

Khazern is a portable and extensible Common Lisp LOOP
implementation. It was originally written by Robert Strandh as part of
[SICL][]. It can be loaded intrinsically in order to replace LOOP in
an existing Lisp implementation or extrinsically to coexist with the
implementation's own LOOP.

To load Khazern intrinsically with [Quicklisp][] do the following:

```lisp
* (ql:quickload :khazern-intrinsic)
* (loop for i in '(1 2 3 4) when (oddp i) collect i)    
(1 3)
```

To load Khazern extrinsically do the following

```lisp
* (ql:quickload :khazern-extrinsic)
* (khazern-extrinsic:loop for i in '(1 2 3 4) when (oddp i) collect i)    
(1 3)
```

[Quicklisp]: https://www.quicklisp.org/beta/
[SICL]: https://github.com/robert-strandh/SICL