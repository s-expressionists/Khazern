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

Khazern also implements an extended LOOP syntax via the
khazern-extension-extrinsic and khazern-extension-intrinsic systems.

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

To use the extension system one need only replace the dependency in
the ASD file.

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

## Khazern Extensions

The orignal Technical Memo 169 "Loop Iteration Macro" which defined
LOOP for Lisp Machine Lisp and Maclisp also specified an extension
mechanism known as an "iteration path." Khazern implements this via
the syntax:

```
for-as-iteration-path ::= {FOR | AS} var [type-spec] BEING
                          {path-exclusive | path-inclusive}
                          {path-using | path-preposition}*
path-exclusive        ::= {EACH | THE} path-name
path-inclusive        ::= form AND {ITS | EACH | HIS | HER}
                          path-name
path-using            ::= USING ({using-name var [type-spec]}+)
path-preposition      ::= preposition-name form
preposition-name      ::= name
using-name            ::= simple-var
path-name             ::= symbol
```

Inclusive iteration paths are those in which the initial value of
`var` was `form` itself. An example given in the Lisp Machine Manual
is that of the CDRS iteration path:

```common-lisp
(loop for x being the cdrs of '(a b c . d) collect x)
; => ((b c . d) (c . d) d)

(loop for x being '(a b c . d) and its cdrs collect x)
; => ((a b c . d) (b c . d) (c . d) d)
```

Then khazern-extension-extrinsic and khazern-extension-intrinsic
systems implement many predefined iteration paths. They are described
in the sections below.

### Hash Table Iteration

#### HASH-KEY Iteration Path

The HASH-KEY iteration path is identical to that described in the ANSI
CL specification with the addition of the ability to specify a
type-spec for USING variable.

```
path-name        ::= {HASH-KEY | HASH-KEYS}
preposition-name ::= {IN | OF}
using-name       ::= {HASH-VALUE}
```

#### HASH-VALUE Iteration Path

The HASH-VALUE iteration path is identical to that described in the
ANSI CL specification with the addition of the ability to specify a
type-spec for USING variable.

```
path-name        ::= {HASH-VALUE | HASH-VALUES}
preposition-name ::= {IN | OF}
using-name       ::= {HASH-KEY}
```

### Sequence Iteration

#### ELEMENTS Iteration Path

The ELEMENTS iteration path iterates over sequences or
multidimensional arrays. If the extensible sequence protocol is
available it will use MAKE-SEQUENCE-ITERATOR from that protocol for
sequences. Otherwise it will attempt to use an efficient sequence
specific iterator with ELT as the fallback iterator.

```
path-name        ::= {ELEMENT | ELEMENTS}
preposition-name ::= {IN | OF | START | END | FROM-END}
using-name       ::= {INDEX | INDICES}
```

* The IN and OF prepositions are synonyms and specify the form that
  evaluates to the sequence. One of these prepositions is required.
* The START preposition specifies the starting index of the
  iteration. It is optional and if not specified it will default to 0.
* The END preposition specifies the ending index of the iteration. It
  is optional and if not specified it will default to the sequence
  length.
* The FROM-END preposition specifies the iteration direction. If it is
  non-NIL then iteration will go from END to START. Otherwise
  iteration will go from START to END. It is optional and if not
  specified it will default to NIL.
* The INDEX or INDICES phrases in USING names a variable or a list of
  variables, in the case of multidimensional arrays, to store the
  current iteration index. It is optional.

### Stream Iteration

All stream iteration paths have the following prepositions and USING
phrases:

* The IN and OF prepositions are synonyms and specify the form that
  evaluates to the stream. If one of these prepositions is not
  specified then \*STANDARD-INPUT\* is used.
* The CLOSE preposition specifies whether the stream should be closed
  when the LOOP is terminated. If it is non-NIL then the stream is
  closed via UNWIND-PROTECT when the loop terminates. It is optional
  and if not specified it will default to NIL
* The STREAM phrase in USING names a variable to store the current
  stream. It is optional.

#### BYTES Iteration Path

The BYTES iteration path iterates over an input stream using
READ-BYTE. It terminates on EOF.

```
path-name        ::= {BYTE | BYTES}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM}
```

#### CHARACTERS Iteration Path

The CHARACTERS iteration path iterates over an input stream using
READ-CHAR. It terminates on EOF.

```
path-name        ::= {CHARACTER | CHARACTERS}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM}
```

#### LINES Iteration Path

The LINES iteration path iterates over an input stream using
READ-LINE. It terminates on EOF.

```
path-name        ::= {LINE | LINES}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM | MISSING-NEWLINE-P}
```

In addition to the standard stream prepositions and USING phrases it
also has the MISSING-NEWLINE-P USING phrase. This phrase is optional
and specifies the name of variable to store the second value returned
from READ-LINE which is non-NIL if the line was not terminated by a
newline.

#### OBJECTS Iteration Path

The OBJECTS iteration path iterates over an input stream using
READ. It terminates on EOF.

```
path-name        ::= {OBJECT | OBJECTS}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM}
```

### Permutation Iteration

#### PERMUTATION Iteration Path

The PERMUTATION iteration path iterates over the permutations of a
sequence.

```
path-name        ::= {PERMUTATION | PERMUTATIONS}
preposition-name ::= {IN | OF}
using-name       ::= {}
```

* The IN and OF prepositions are a sequence to permute. On each loop
  step a copy of this sequence is made with the elements permuted.

#### COMBINATION Iteration Path

The COMBINATION iteration path iterates over the combinations of a
sequence.

```
path-name        ::= {COMBINATION | COMBINATIONS}
preposition-name ::= {IN | OF | CHOOSE}
using-name       ::= {}
```

* The IN and OF prepositions are a sequence to permute. On each loop
  step a copy of this sequence is made with the elements permuted.
* The CHOOSE prepositions is an integer that specifies the length of
  the subsequence to select.

#### TUPLE Iteration Path

The TUPLE iteration path iterates over the all possible tuples of a
collection of sequences. It is essentially the Cartesian Product.

```
path-name        ::= {TUPLE | TUPLES}
preposition-name ::= {IN | OF}
using-name       ::= {}
```

* The IN and OF prepositions are a sequence of sequences. Instead of
  individual sequences non-negative integers are also permitted which
  imply a sequence of that length with integers less than the length.

### Value Accumulation

#### COLLECT/APPEND/NCONC Sequence Extensions

The value accumulation clauses COLLECT, COLLECTING, APPEND, APPENDING,
NCONC, and NCONCING have all been extended to include type
specifications for arbitrary sequences.

```common-lisp
(kee:loop for i below 10
          collect i of-type (vector fixnum))
; => #(0 1 2 3 4 5 6 7 8 9)
(kee:loop for i below 10
          append (list i (- i)) of-type simple-vector)
; => #(0 0 1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8 9 -9)
(kee:loop for i from 65 to 90
          collect (code-char i) of-type string)
; => "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
```

#### Set Accumulation

The set accumulation clauses ADJOIN, ADJOINING, DISJOIN, DISJOINING,
UNION, UNIONING, NUNIONING, INTERSECTION, INTERSECTING, NINTERSECTION,
NINSTERSECTING, DIFFERENCE, NDIFFERENCE, DIFFERENCING, and
NDIFFERENCING have been added.

All of these accumulation clauses allow the optional prepositions KEY
and TEST as in the :KEY and :TEST keyword arguments to CL:ADJOIN.

```common-lisp
(kee:loop for ch across "The quick brown fox jumps over the lazy dog."
          when (alpha-char-p ch)
            adjoin ch test #'char-equal)
; => (#\T #\h #\e #\q #\u #\i #\c #\k #\b #\r #\o #\w #\n #\f #\x #\j #\m #\p
;     #\s #\v #\l #\a #\z #\y #\d #\g)
(kee:loop for ch across "The quick brown fox jumps over the lazy dog."
          when (alpha-char-p ch)
            adjoin ch of-type string test #'char-equal)
; => "Thequickbrownfxjmpsvlazydg"
(kee:loop for i below 10
          union (list (mod i 3) (mod i 5)))
; => (0 1 2 3 4)
```

#### Merge Accumulation

A MERGE/MERGING accumulation clause has been added that uses
CL:MERGE. It has a required preposition of PREDICATE and an optional
KEY preposition.

```common-lisp
(kee:loop for i below 10
          merge (list (- i) i) of-type vector predicate #'<)
; => #(-9 -8 -7 -6 -5 -4 -3 -2 -1 0 0 1 2 3 4 5 6 7 8 9)
```

[Quicklisp]: https://www.quicklisp.org/beta/
[SICL]: https://github.com/robert-strandh/SICL