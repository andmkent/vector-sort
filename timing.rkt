#lang racket/base

(require (rename-in "sort.rkt" [raw-vector-sort vector-sort])
         racket/vector)

(define-syntax-rule (iter . body)
  (for ([_ (in-range 10000)])
    . body))

;; ********************************
(define lengths '(10 50 100 1000 10000))

;; lists of random numbers
(define (run-tests title fn)
  (define ls
    (for/list ([len (in-list lengths)])
      (build-list len fn)))
  (printf "- - - - - - - ~a tests - - - - - - -\n" title)
  (for ([len (in-list lengths)]
        [l (in-list ls)])
    (printf "SIZE: ~a\n" len)
    (printf "sort:\n")
    (time (iter (sort l <)))
    (printf "vector-sort:\n")
    (time (iter (vector-sort (list->vector l) <)))
    (unless (equal? (sort l < )
                    (vector->list (vector-sort (list->vector l) <)))
      (error "not equal!")))
  (printf "- - - - - end of ~a tests - - - - -\n" title))

(run-tests "random" (λ _ (random 100000)))
(run-tests "sorted" (λ (n) n))
(run-tests "mostly sorted" (λ (n) (if (zero? (modulo n 7))
                                      (random 100000)
                                      n)))
(run-tests "reverse sorted" (λ (n) (* -1 n)))
(run-tests "many duplicates" (λ (n) (modulo n 7)))