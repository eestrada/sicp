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

"Exercise 2.27: deep reverse function"

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
