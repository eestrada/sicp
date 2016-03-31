(define (square x) (* x x))
(define nil '())

(define (last-pair l)
  "Exercise 2.17: return last pair of a list"
  (if (eq? (cdr l) '())
      l
      (last-pair (cdr l))))

(define (reverse2 l)
  "Exercise 2.18: procedure to reverse a list"
  (define (reverse2-iter f r)
    (if (eq? f '())
        r
        (reverse2-iter (cdr f) (cons (car f) r))))
  (reverse2-iter l '()))

(define (same-parity first . rest)
  "Exercise 2.20: using dotted-tail notation, return only arguments
   that have the same even-odd parity as the first argument."
  (define (sp-iter first-parity l r)
    (if (eq? r '())
        l
        (let ((newr (cdr r)))
            (if (eq? first-parity (remainder (car r) 2))
                (sp-iter first-parity
                         (append! l (list (car r)))
                         newr)
                (sp-iter first-parity l newr)))))
  (sp-iter (remainder first 2)
           (list first)
           rest))

(define (square-list1 items)
  "Exercise 2.21: return squared list (using explicit recursion)"
  (if (null? items)
      '()
      (cons (square (car items)) (square-list1 (cdr items)))))

(define (square-list2 items)
  "Exercise 2.21: return squared list (using map)"
  (map square items))

(define (square-list-bad1 items)
  "Exercise 2.22: this doesn't work because he is using cons which builds
   the list in reverse since const can only know the head of the list and
   the only place to append a pair is to the front."
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items nil))

(define (square-list-bad2 items)
  "Exercise 2.22: this doesn't work because he is placing a list as
   the first item in the pair when he is consing. Basically, he is making
   a really weird tree instead of a list."
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items nil))

"Exercise 2.22: to get this to work, he should instead use the append or
the append! functions to append new values to the END of the list."

(define (for-each2 proc items)
  "Exercise 2.23: implement for-each (and mine even does
   tail-call optimization :)"
  (if (null? items)
      items
      (let ()
        (proc (car items))
        (for-each2 proc (cdr items)))))

"Exercise 2.24: I figured out the first part (the interpreters representation) but not
the second part (box and pointer); that part I went to scheme wiki to
undestand"

'(1 (2 (3 4)))

" ;; from scheme wiki at (http://community.schemewiki.org/?sicp-ex-2.24)
   +---+---+  +---+---+
   | * | *-+->| * | / |
   +-+-+---+  +-+-+---+
     |          |
     V          V
   +---+      +---+---+  +---+---+
   | 1 |      | * | *-+->| * | / |
   +---+      +-+-+---+  +---+---+
                |          |
                V          V
              +---+      +---+---+  +---+---+
              | 2 |      | * | *-+->| * | / |
              +---+      +-+-+---+  +-+-+---+
                           |          |
                           V          V
                         +---+      +---+
                         | 3 |      | 4 |
                         +---+      +---+
"

"Exercise 2.25: get the number 7 from each list structure"
;; (1 3 (5 7) 9)
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
;; ((7))
(car (car '((7))))
;; (1 (2 (3 (4 (5 (6 7))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
(cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))

"Exercise 2.26: determine what the evaluation would be of append, cons and list"
(append '(1 2 3) '(4 5 6)) ;; -> (1 2 3 4 5 6)
(cons '(1 2 3) '(4 5 6)) ;; -> ((1 2 3) 4 5 6)
;; I initially assumed the above would result in ((1 2 3) . (4 5 6)) forgetting
;; that if the cdr of a pair is another list, really it is just pointing to
;; another pair, meaning the list is simply extended.
(list '(1 2 3) '(4 5 6)) ;; -> ((1 2 3) (4 5 6))

(define (deep-reverse l)
  "Exercise 2.27: deep reverse function"
  (define (reverse2-iter f r)
    (if (eq? f '())
        r
        (let ((head (if (list? (car f))
                        (reverse2-iter (car f) '())
                        (car f))))
          (reverse2-iter (cdr f) (cons head r)))))
  (reverse2-iter l '()))

(define (fringe tree)
  "Exercise 2.28: function to flatten a tree, left to right

   This is just a specialized version of count-leaves from the book."
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

"Exercise 2.29: mobile functions."
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (car (cdr mobile)))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (car (cdr branch)))

(define (total-weight mobile)
  (let ((left (branch-structure (left-branch mobile)))
        (right (branch-structure (right-branch mobile))))
  (+ (if (number? left)
         left
         (total-weight left))
     (if (number? right)
         right
         (total-weight right)))))

(define (mobile-balanced? mobile)
  (define left (branch-structure (left-branch mobile)))
  (define right (branch-structure (right-branch mobile)))
  (= (if (number? left)
         left
         (total-weight left))
     (if (number? right)
         right
         (total-weight right))))

"Exercise 2.29: mobile functions changed by using cons instead of list."
(define (right-branch2 mobile)
  (cdr mobile))

(define (branch-structure2 branch)
  (cdr branch))

(define (square-tree tree)
  "Exercise 2.30: function to square values in a tree

   This is just a specialized version of count-leaves from the book."
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree2 tree)
  "Exercise 2.30: function to square values in a tree, but using map

   This is just a specialized version of scale-tree from the book."
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree2 sub-tree)
             (square sub-tree)))
       tree))

(define (tree-map func tree)
  "Exercise 2.31: map a function on a tree"
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map func sub-tree)
             (func sub-tree)))
       tree))

(define (subsets s)
  "Exercise 2.32: create a set of subsets

   All the work happens when we are unwinding the stack. Basically, we
   prepend the first item of `s` onto the return set of subsets. Initially,
   the only subset is the empty set `'()`. Then 3 is prepended to the empty
   set and appended to the set of subsets, so then we have `'(() (3))` since
   prepending (aka consing) anything to a list, even an empty one, just
   creates another list. This same process is repeated for the numbers `2`
   and then `1`.

   It took me a while to understand this one and I needed to look at the
   following URL to do even that:
   http://community.schemewiki.org/?sicp-ex-2.32"
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (v) (cons (car s) v))
                          rest)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate-recursive op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate-recursive op initial (cdr sequence)))))

(define (accumulate-iterate op initial sequence)
  "Doesn't quite work right yet..."
  (if (null? sequence)
      initial
      (accumulate-iterate op
                          (op (car sequence) initial)
                          (cdr sequence))))

(define accumulate accumulate-recursive)

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

"Exercise 2.33: Fill in the missing expressions to complete the
following definitions of some basic list-manipulation operations as
accumulations:"

(define (accu-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (accu-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (accu-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(define (horner-eval x coefficient-sequence)
  "Exercise 2.34: Horner evaluation.

   My original solution was all mixed up. I didn't figure it out until
   I read the explanations at: http://community.schemewiki.org/?sicp-ex-2.34"
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

(define (accu-count-leaves t)
  "Exercise 2.35: count-leaves using accumulate

   This particular exercise felt forced. It is neither easier nor
   clearer to use accumulate to accomplish this. As I started to solve
   this I thought 'It can't be done this way. It is too asinine.' Then I
   checked schemewiki and found that the solutions there were just as
   silly as my own. This exercise felt like a problem for the sake of one.
   :unamused:"
  (accumulate +
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

(define (accumulate-n op init seqs)
  "Exercise 2.36: accumulate a list of lists"
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

"Exercise 2.37: define matrix operations"

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product row v))
       m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

"Exercise 2.38: fold-left and fold-right"

(define fold-right accumulate)

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; what is the result of the following?
(fold-right / 1 (list 1 2 3))       ;; 3 / 1 = 3, 2 / 3 = 2/3, 1 / 2/3 -> 3/2
(fold-left / 1 (list 1 2 3))        ;; 1 / 1 = 1, 1 / 2 = 1/2, 1/2 / 3 -> 1/6
(fold-right list nil (list 1 2 3))  ;; (1 (2 (3 ())))
(fold-left list nil (list 1 2 3))   ;; (((() 1) 2) 3)
;; in order for an operation to be the same in either fold-left of fold-right
;; the operation must be commutative (and possibly associative as well?).
