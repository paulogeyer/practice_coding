(define nil '())

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
	count
	(length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; ex 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;; ex 2.18
(define (reverse lst1)
  (define (reverse-iter lst1 lst2)
    (if (null? lst1)
	lst2
	(reverse-iter (cdr lst1) (cons (car lst1) lst2))))
  (reverse-iter (cdr lst1) (cons (car lst1) '())))

;; ex 2.19
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-mode? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
	((or (< amount 0) (no-mode? coin-values)) 0)
	(else
	 (+ (cc amount
		(except-first-denomination coin-values))
	    (cc (- amount
		   (first-denomination coin-values))
		coin-values)))))

(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (same-parity . lst)
  (define (parity-iter first lst)
    (if (null? lst)
	'()
	(if (= (remainder first 2)
	       (remainder (car lst) 2))
	    (cons (car lst) (parity-iter first (cdr lst)))
	    (parity-iter first (cdr lst)))))
  (parity-iter (car lst) lst))

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
	    (scale-list (cdr items) factor))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
	    (map proc (cdr items)))))

(define (scale-int items factor)
  (map (lambda (x) (* x factor))
       items))

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (square x))
       items))

;; ex 2.22

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons (square (car things))
		    answer))))
  (iter items nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (cons answer
		    (square (car things))))))
  (iter items nil))

(define (square-list items)
  (define (append lst1 lst2)
    (if (null? lst1)
	lst2
	(cons (car lst1)
	      (append (cdr lst1) lst2))))
  (define (iter things answer)
    (if (null? things)
	answer
	(iter (cdr things)
	      (append answer (list (square (car things)))))))
  (iter items nil))

;; (cons 1 (cons 4 (cons 9 (cons 16 (cons 25 nil)))))

;; ex 2.23
(define (for-each proc lst)
  (cond ((null? lst)
	 true)
	(else
	 (proc (car lst))
	 (for-each proc (cdr lst)))))

(define (count-leaves x)
  (cond ((null? x) 0)
	((not (pair? x)) 1)
	(else (+ (count-leaves (car x))
		 (count-leaves (cdr x))))))

;; ex 2.27

(define (reverse lst1)
  (define (reverse-iter lst1 lst2)
    (if (null? lst1)
	lst2
	(reverse-iter (cdr lst1) (cons (reverse (car lst1)) lst2))))
  (if (list? lst1)
      (reverse-iter (cdr lst1) (cons (reverse (car lst1)) '()))
      lst1))

;; ex 2.28
(define (fringe tree)
  (if (null? tree)
      nil
      (let ((l1 (car tree))
	    (l2 (cdr tree)))
	(if (null? l1)
	    (fringe l2)
	    (if (list? l1)
		(append l1 (fringe l2))
		(list l1))))))

(define x (list (list 1 2) (list 3 4)))
