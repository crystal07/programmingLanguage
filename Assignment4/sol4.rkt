#lang racket

(define tree1 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
(define tree11 '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (11 () (14 (13 () ()) ()))))
(define tree2 '(6 (4 ()()) (7 ()())))
(define tree3 '(6 (7 ()()) (8 ()())))
(define tree4 '(5 () (8 () () ())))
(define tree5 '(5 () (2 () () ())))
(define tree6 '(5 (3 () ()) ()))
(define tree7 '(5 (7 () ()) ()))
(define tree8 '(5 () ()))

(define (check_bst t)
      (let ([left (car (cdr t))] [right (car (cdr (cdr t)))])
        (cond
          [(null? t) #t]
          [(and (null? left) (null? right)) #t]
          [(and (null? left) (< (car t) (car right))) (check_bst right)]
          [(null? left) #f]
          [(and (null? right) (> (car t) (car left))) (check_bst left)]
          [(null? right) #f]
          [(< (car t) (car left)) #f]
          [(> (car t) (car right)) #f]
          [#t (and (check_bst left) (check_bst right))]
        )
      )
  
)

;(check_bst tree1)
;(check_bst tree2)
;(check_bst tree3)
;(check_bst tree4)
;(check_bst tree5)
;(check_bst tree6)
;(check_bst tree7)
;(check_bst tree8)

(define (apply f t)
      (let ([left (car (cdr t))] [right (car (cdr (cdr t)))])
        (cond
          [(null? t) (list '())]
          [(and (null? left) (null? right)) (list (f (car t)) '() '())]
          [(null? left) (list (f (car t)) '() (apply f right))]
          [(null? right) (list (f (car t)) (apply f left) '())]
          [#t (list (f (car t)) (apply f left) (apply f right))]
        )
      )
)

;(apply (lambda (v) (+ v 1)) tree1)

(define (equals t1 t2)
  ( cond
      [(null? t1) (if (null? t2) #t #f)]
      [(null? t2) #f]
      [(= (car t1) (car t2)) (and (equals (car (cdr t1)) (car (cdr t2))) (equals (car (cdr (cdr t1))) (car (cdr (cdr t2)))))]
      [#t #f]
    
  )
)

(equals tree1 tree1)
(equals tree1 tree2)
(equals tree1 tree11)