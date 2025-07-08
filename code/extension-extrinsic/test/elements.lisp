(cl:in-package #:khazern-extension-extrinsic/test)

(define-test elements/vector-01
    (is equal
        '(a b c d e f g)
        (kee:loop for i being the elements in #(a b c d e f g)
                  collect i)))

(define-test elements/vector-02
    (is equal
        '(a 0 b 1 c 2 d 3 e 4 f 5 g 6)
        (kee:loop for i being the elements in #(a b c d e f g) using (index j)
                  collect i
                  collect j)))

(define-test elements/vector-03
    (is equal
        '(b 1 c 2 d 3 e 4 f 5 g 6)
        (kee:loop for i being the elements in #(a b c d e f g) start 1 using (index j)
                  collect i
                  collect j)))

(define-test elements/vector-04
    (is equal
        '(b 1 c 2 d 3 e 4)
        (kee:loop for i being the elements in #(a b c d e f g) start 1 end 5 using (index j)
                  collect i
                  collect j)))

(define-test elements/vector-05
    (is equal
        '(a 0 b 1 c 2 d 3 e 4)
        (kee:loop for i being the elements in #(a b c d e f g) end 5 using (index j)
                  collect i
                  collect j)))

(define-test elements/vector-06
    (is equal
        '(g 6 f 5 e 4 d 3 c 2 b 1 a 0)
        (kee:loop for i being the elements in #(a b c d e f g) using (index j) from-end t
                  collect i
                  collect j)))

(define-test elements/vector-07
    (is equal
        '(g 6 f 5 e 4 d 3 c 2 b 1)
        (kee:loop for i being the elements in #(a b c d e f g) start 1 from-end t
                    using (index j)
                  collect i
                  collect j)))

(define-test elements/vector-08
    (is equal
        '(e 4 d 3 c 2 b 1)
        (kee:loop for i being the elements in #(a b c d e f g) start 1 end 5 from-end t
                    using (index j)
                  collect i
                  collect j)))

(define-test elements/vector-09
    (is equal
        '(e 4 d 3 c 2 b 1 a 0)
        (kee:loop for i being the elements in #(a b c d e f g) end 5 from-end t using (index j)
                  collect i
                  collect j)))

(define-test elements/list-01
    (is equal
        '(a b c d e f g)
        (kee:loop for i being the elements in '(a b c d e f g)
                  collect i)))

(define-test elements/list-02
    (is equal
        '(a 0 b 1 c 2 d 3 e 4 f 5 g 6)
        (kee:loop for i being the elements in '(a b c d e f g) using (index j)
                  collect i
                  collect j)))

(define-test elements/list-03
    (is equal
        '(b 1 c 2 d 3 e 4 f 5 g 6)
        (kee:loop for i being the elements in '(a b c d e f g) start 1 using (index j)
                  collect i
                  collect j)))

(define-test elements/list-04
    (is equal
        '(b 1 c 2 d 3 e 4)
        (kee:loop for i being the elements in '(a b c d e f g) start 1 end 5 using (index j)
                  collect i
                  collect j)))

(define-test elements/list-05
    (is equal
        '(a 0 b 1 c 2 d 3 e 4)
        (kee:loop for i being the elements in '(a b c d e f g) end 5 using (index j)
                  collect i
                  collect j)))

(define-test elements/list-06
    (is equal
        '(g 6 f 5 e 4 d 3 c 2 b 1 a 0)
        (kee:loop for i being the elements in '(a b c d e f g) using (index j) from-end t
                  collect i
                  collect j)))

(define-test elements/list-07
    (is equal
        '(g 6 f 5 e 4 d 3 c 2 b 1)
        (kee:loop for i being the elements in '(a b c d e f g) start 1 from-end t
                    using (index j)
                  collect i
                  collect j)))

(define-test elements/list-08
    (is equal
        '(e 4 d 3 c 2 b 1)
        (kee:loop for i being the elements in '(a b c d e f g) start 1 end 5 from-end t
                    using (index j)
                  collect i
                  collect j)))

(define-test elements/list-09
    (is equal
        '(e 4 d 3 c 2 b 1 a 0)
        (kee:loop for i being the elements in '(a b c d e f g) end 5 from-end t using (index j)
                  collect i
                  collect j)))
