(define (last-pair l)
  "Excercise 2.17: return last pair of a list"
  (if (eq? (cdr l) '())
      l
      (last-pair (cdr l))))

(define (reverse2 l)
  "Excercise 2.18: procedure to reverse a list"
  (define (reverse2-iter f r)
    (if (eq? f '())
        r
        (reverse2-iter (cdr f) (cons (car f) r))))
  (reverse2-iter l '()))
