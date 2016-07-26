#lang racket/base

(require racket/unsafe/ops
         (for-syntax racket/base))

;; common vector/integer ops & shorthands
(define-syntax i+ (make-rename-transformer #'+))
(define-syntax i- (make-rename-transformer #'-))
(define-syntax-rule (i*2 i) (* i 2))
(define-syntax-rule (i/2 i) (quotient i 2))
(define-syntax i< (make-rename-transformer #'<))
(define-syntax i≤ (make-rename-transformer #'<=))
(define-syntax i= (make-rename-transformer #'=))
(define-syntax vref (make-rename-transformer #'vector-ref))
(define-syntax vset! (make-rename-transformer #'vector-set!))

;; while macro for concise incrementing/decrementing of variables
(define-syntax (while stx)
  (syntax-case stx ()
    [(_ cond body ... #:++ i #:else return)
     #'(let loop ([i i])
         (if cond (let () body ... (loop (i+ i 1))) return))]
    [(_ cond body ... #:++ i)
     #'(let loop ([i i])
         (if cond (let () body ... (loop (i+ i 1))) (void)))]
    [(_ cond body ... #:-- i #:else return)
     #'(let loop ([i i])
         (if cond (let () body ... (loop (i- i 1))) return))]
    [(_ cond body ... #:-- i)
     #'(let loop ([i i])
         (if cond (let () body ... (loop (i- i 1))) (void)))]))

;; vector value swap macro
(define-syntax-rule (swap! A i j)
  (let ([tmp (vref A i)])
    (vset! A i (vref A j))
    (vset! A j tmp)))

;; generic heapsort helpers
(define-syntax-rule (parent i) (i/2 (i- i 1)))
(define-syntax-rule (lchild i) (i+ (i*2 i) 1))
(define-syntax-rule (rchild i) (i+ (i*2 i) 2))


(define <? <)

(define heapsort!
  (λ (A low high)
    (define (ref i)
      (vref A (i+ i low)))
    (define (swap! i j)
      (let ([i (i+ i low)]
            [j (i+ j low)])
        (let ([tmp (vref A i)])
          (vset! A i (vref A j))
          (vset! A j tmp))))
    
    (define size (i+ (i- high low) 1))
    
    (define (sift-down! r end)
      (define c (+ (* 2 r) 1))
      (define c+1 (+ c 1))
      (when (<= c end)
        (define child 
          (if (and (<= c+1 end) (< (ref c) (ref c+1)))
              c+1 c))
        (when (< (ref r) (ref child))
          (swap! r child))
        (sift-down! child end)))
    
    (for ([i (in-range (quotient (- size 2) 2) -1 -1)])
      (sift-down! i (- size 1)))
    
    (for ([end (in-range (- size 1) 0 -1)])
      (swap! 0 end)
      (sift-down! 0 (- end 1)))))
 

(define v1 (vector 2 3 1))
(heapsort! v1 0 2)
v1

(define v2 (vector 99 0 2 1 5 3 4 99 100))
(heapsort! v2 0 8)
v2
