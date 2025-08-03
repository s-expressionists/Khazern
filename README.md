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
mechanism known as an "iteration path." The syntax was roughly:

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

Usage of inclusive iteration path seems to have been rare even in the
Lisp Machine Lisp. The mechanism is still exists in many CL
implementations but is completely unused in modern code. Only the
residual syntax of exclusive iteration paths survived in the ANSI CL
specification. For this reason, Khazern only supports exclusive
iteration path extensions. Khazern's syntax is:

```
for-as-being     ::= {FOR | AS} var [type-spec] BEING {EACH | THE}?
                     name {path-using | path-preposition}*
path-using       ::= USING ({using-name var [type-spec]}+)
path-preposition ::= preposition-name form
preposition-name ::= name
using-name       ::= simple-var
name             ::= symbol
```

In the original path iteration syntax EACH or THE is needed to
distinguish between inclusive and exclusive paths. Since Khazern
doesn't have inclusive paths, the EACH and THE tokens are made
optional. Also, path-using permits optional type-specs which was not
allowed in the original iteration path implementations.

Khazern supports many extensions mechanisms in addition to iteration
paths. The primary interface to adding extensions is PARSE-CLAUSE
which takes as one of its arguments a REGION that specifies where in
LOOP the clause is permitted. Currently there are the following
regions:

* body-region - top-level clauses in LOOP.
* selectable-region - clauses that can occur in conditionals or as
  top-level clauses.
* for-as-region - subclauses of FOR or AS.
* being-region - subclauses of FOR-BEING or AS-BEING.
* with-region - subclauses of WITH.

This makes iteration paths valid in the being-region as so Khazern
refers to these as being clauses.

Then khazern-extension-extrinsic and khazern-extension-intrinsic
systems implement many predefined extensions. They are described
in the sections below.

### Hash Table Iteration

#### HASH-KEYS Being Clause

The HASH-KEYS being clause is identical to that described in the ANSI
CL specification with the addition of the ability to specify a
type-spec for USING variable.

```
path-name        ::= {HASH-KEY | HASH-KEYS}
preposition-name ::= {IN | OF}
using-name       ::= {HASH-VALUE}
```

#### HASH-VALUES Being Clause

The HASH-VALUES being clause is identical to that described in the
ANSI CL specification with the addition of the ability to specify a
type-spec for USING variable.

```
path-name        ::= {HASH-VALUE | HASH-VALUES}
preposition-name ::= {IN | OF}
using-name       ::= {HASH-KEY}
```

#### ENTRIES Being Clause

The dictionary entry for MAPHASH has the statement that it "Iterates
over all entries in the hash-table." In other words the key value
pairs are considered an iterable quantity known as an "entry." This
makes the HASH-KEYS and HASH-VALUES being clauses seem a bit awkward
with their USING syntax. The original iteration path mechanism treated
USING as a way to access internal loop variables or at best auxilary
variables. The hash keys or values are not really auxilary values.

To emphasize the "entry" concept for hash tables and simply the loop
syntax for hash tables, Khazern supplies the ENTRIES being clause. The
stepping variable has the value of a list with the first element set
to the hash table key and the second element set to the hash table
value. This enables LOOP's built in destructuring to handle the
extraction.

```
path-name        ::= {ENTRY | ENTRIES}
preposition-name ::= {IN | OF}
using-name       ::= {}
```

The ENTRIES being clause also has an abbreviated form via the OF
token.

```
for-as-elements  ::= {FOR | AS} var [type-spec] OF form
```

##### Examples

```common-lisp
(kee:loop for (k v) being entries of ht collect k collect v)
; => (:FU :BAR :BAZ :QUUX)
(kee:loop for (k v) of ht collect k collect v)
; => (:FU :BAR :BAZ :QUUX)
(kee:loop for (k v) of-type (symbol symbol) being entries of ht collect k collect v)
; => (:FU :BAR :BAZ :QUUX)
```

### Package Symbol Iteration

#### SYMBOLS, PRESENT-SYMBOLS, and EXTERNAL-SYMBOLS Being Clauses

SYMBOLS, PRESENT-SYMBOLS, and EXTERNAL-SYMBOLS being clauses are
identical to that described in the ANSI CL specification except they
permit the IN/OF preposition form to be a list of package designators
as WITH-PACKAGE-ITERATOR does. They also add the ASSESSIBILITY-TYPE
and PACKAGE USING variables to access those values returned by the
package iterator.

```
path-name        ::= {SYMBOL | SYMBOLS | PRESENT-SYMBOL |
                      PRESENT-SYMBOLS | EXTERNAL-SYMBOL |
                      EXTERNAL-SYMBOLS}
preposition-name ::= {IN | OF}
using-name       ::= {ACCESSIBILITY-TYPE | PACKAGE}
```

### Sequence Iteration

#### ELEMENTS Being Clause

The ELEMENTS being clause iterates over sequences or
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

The ELEMENTS being clause also has an abbreviated form via the OVER
token.

```
for-as-elements  ::= {FOR | AS} var [type-spec] OVER form
                     {path-using | path-preposition}*
path-using       ::= USING ({using-name var [type-spec]}+)
path-preposition ::= preposition-name form
preposition-name ::= {START | END | FROM-END}
using-name       ::= {INDEX | INDICES}
```

### Stream Iteration

All stream being clauses have the following prepositions and USING
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

#### BYTES Being Clause

The BYTES being clause iterates over an input stream using
READ-BYTE. It terminates on EOF.

```
path-name        ::= {BYTE | BYTES}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM}
```

#### CHARACTERS Being Clause

The CHARACTERS being clause iterates over an input stream using
READ-CHAR. It terminates on EOF.

```
path-name        ::= {CHARACTER | CHARACTERS}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM}
```

#### LINES Being Clause

The LINES being clause iterates over an input stream using
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

#### OBJECTS Being Clause

The OBJECTS being clause iterates over an input stream using
READ. It terminates on EOF.

```
path-name        ::= {OBJECT | OBJECTS}
preposition-name ::= {IN | OF | CLOSE}
using-name       ::= {STREAM}
```

### Cons Iteration

#### CARS Being Clause

The CARS being clause behaves the same as FOR-AS-IN-LIST, but is
included to mirror the naming of MAPCAR and MAPLIST.

```
path-name        ::= {CAR | CARS}
preposition-name ::= {IN | OF}
using-name       ::= {}
```

#### LISTS Being Clause

The LISTS being clause behaves the same as FOR-AS-ON-LIST, but is
included to mirror the naming of MAPCAR and MAPLIST.

```
path-name        ::= {LIST | LISTS}
preposition-name ::= {IN | OF}
using-name       ::= {}
```

### Permutation Iteration

#### PERMUTATIONS Being Clause

The PERMUTATIONS being clause iterates over the permutations of a
sequence.

```
path-name        ::= {PERMUTATION | PERMUTATIONS}
preposition-name ::= {IN | OF}
using-name       ::= {}
```

* The IN and OF prepositions are a sequence to permute. On each loop
  step a copy of this sequence is made with the elements permuted.

#### COMBINATIONS Being Clause

The COMBINATIONS being clause iterates over the combinations of a
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

#### MULTICOMBINATIONS Being Clause

The MULTICOMBINATIONS being clause iterates over the combinations of a
sequence with repetition allowed.

```
path-name        ::= {MULTICOMBINATION | MULTICOMBINATIONS}
preposition-name ::= {IN | OF | CHOOSE}
using-name       ::= {}
```

* The IN and OF prepositions are a sequence to permute. On each loop
  step a copy of this sequence is made with the elements permuted.
* The CHOOSE prepositions is an integer that specifies the length of
  the subsequence to select.

#### TUPLES Being Clause

The TUPLES being clause iterates over the all possible tuples of a
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

### Subform Accumulation

The USE binding clause enables the use of value accumulation in subforms
nested inside of LOOP. The syntax is:

```
use-clause       ::= USE function-name = accumulation-name
                     [INTO simple-var] [type-spec]
function-name     ::= symbol
accumulation-name ::= {ADJOIN | ADJOINING | APPEND | COLLECT |
                       DIFFERENCE | DIFFERENCING | DISJOIN | 
                       DISJOINING | INTERSECTING | INTERSECTION | 
                       MERGE | NCONC | NDIFFERENCE | NDIFFERENCING |
                       NINTERSECTING | NINTERSECTION | NUIONION | 
                       UNION | UNIONING}
```

A local function will then be created with "function-name" that takes
a single argument that will be accumulated, along with key arguments for
any prepositions.

```common-lisp
(kee:loop use my-collect = collect
          for i below 10
          do (my-collect i))
; => (0 1 2 3 4 5 6 7 8 9)
(kee:loop use my-adjoin = adjoin
          for ch across "The quick brown fox jumps over the lazy dog."
          when (alpha-char-p ch)
            do (my-adjoin ch :test #'char-equal))
; => (#\T #\h #\e #\q #\u #\i #\c #\k #\b #\r #\o #\w #\n #\f #\x #\j #\m
;     #\p #\s #\v #\l #\a #\z #\y #\d #\g)
(kee:loop use my-max = maximize into q
          for i from 1 upto 10
          finally (return q)
          do (my-max (random i)))
; => 7
```

### CLEANUP Clause

The FINALLY clause of CL:LOOP is not guaranteed to be executed and
therefore is not a good place to put cleanup forms from bindings
introduced by WITH clauses. khazern-extension has the CLEANUP clause
for this. It has identical syntax as the FINALLY clause, but it places
its forms inside an UNWIND-PROTECT.

```common-lisp
(macroexpand-1 '(kee:loop with x = (make-fubar)
                          cleanup (release-fubar x)))
; => (BLOCK NIL
;      (LET ((X NIL) (#:FORM3400 (MAKE-FUBAR)))
;        (DECLARE (IGNORABLE X))
;        (SETQ X #:FORM3400)
;        (UNWIND-PROTECT
;            (TAGBODY
;             #:BODY3401
;              (GO #:BODY3401)
;             KHAZERN-EXTENSION-EXTRINSIC::EPILOGUE
;              (RETURN-FROM NIL NIL))
;          (RELEASE-FUBAR X))))
```

[Quicklisp]: https://www.quicklisp.org/beta/
[SICL]: https://github.com/robert-strandh/SICL