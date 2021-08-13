(define nil '())

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1))
	 (fib (- n 2)))))

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

;; ex 2.29

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
  (if (null? mobile)
      0
      (let ((lb (left-branch mobile))
	    (rb (right-branch mobile)))
	(+ (if (list? (branch-structure lb))
	       (total-weight (branch-structure lb))
	       (branch-structure lb))
	   (if (list? (branch-structure rb))
	       (total-weight (branch-structure rb))
	       (branch-structure rb))))))

(define (balanced mobile)
  (let ((lb (left-branch mobile))
	(rb (right-branch mobile)))
    (and (= (* (branch-length lb)
	       (if (list? (branch-structure lb))
		   (total-weight (branch-structure lb))
		   (branch-structure lb)))
	    (* (branch-length rb)
	       (if (list? (branch-structure rb))
		   (total-weight (branch-structure rb))
		   (branch-structure rb))))
	 (and (if (list? (branch-structure lb))
		  (balanced (branch-structure lb)) true)
	      (if (list? (branch-structure rb))
		  (balanced (branch-structure rb)) true)))))

(define m (make-mobile (make-branch 10 3)
		       (make-branch 8 3)))

(define m-balanced (make-mobile (make-branch 8 3)
				(make-branch 8 3)))

(define m-balanced2 (make-mobile (make-branch 2 3)
				 (make-branch 3 2)))

(define m1 (make-mobile (make-branch 10 2)
			(make-branch 8 (make-mobile (make-branch 3 5)
						    (make-branch 4 5)))))

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
	((not (pair? tree)) (* tree factor))
	(else (cons (scale-tree (car tree) factor)
		    (scale-tree (cdr tree) factor)))))

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (scale-tree sub-tree factor)
	     (*  sub-tree factor)))
       tree))

(define tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;; ex 2.30

(define (square-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (square tree))
	(else (cons (square-tree (car tree))
		    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (square-tree sub-tree)
	     (square sub-tree)))
       tree))

;; ex 2.31

(define (tree-map proc tree)
  (map (lambda (sub-tree)
	 (if (pair? sub-tree)
	     (tree-map proc sub-tree)
	     (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))

;; ex 2.32
;; TODO: got an weird output

(define (identity x) x)

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
	(append rest (map (lambda (x) (cons (car s) x)) rest)))))

;; 2.2.3 sequences as conventional interfaces

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
	((not (pair? tree))
	 (if (odd? tree) (square tree) 0))
	(else (+ (sum-odd-squares (car tree))
		 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
	nil
	(let ((f (fib k)))
	  (if (even? f)
	      (cons f (next (+ k 1)))
	      (next (+ k 1))))))
  (next 0))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
	((predicate (car sequence))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

;; (accumulate + 0 (list 1 2 3 4 5 6))
;; (accumulate * 1 (list  1 2 3 4 5))
;; (accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
	((not (pair? tree)) (list tree))
	(else (append (enumerate-tree (car tree))
		      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
	      0
	      (map square
		   (filter odd?
			   (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
	      nil
	      (filter even?
		      (map fib
			   (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
	      nil
	      (map square
		   (map fib
			(enumerate-interval 0 n)))))

(define (list-fibs n)
  (accumulate cons
	      nil
	      (map fib
		   (enumerate-interval 0 n))))

(define (product-of-square-of-odd-elements sequence)
  (accumulate *
	      1
	      (map square
		   (filter odd? sequence))))

;; ex 2.33

(define (map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
	      nil
	      sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

;;  ex 2.34
;; TODO: is this right?

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
		(* (+ (* this-coeff x) higher-terms) x))
	      0
	      coefficient-sequence))

;; ex 2.35

(define (count-leaves t)
  (accumulate +
	      0
	      (map (lambda (x) (if (pair? x)
				   (count-leaves x)
				   (if (null? x)
				       0
				       1)))
		   t)))

(define x (cons (list 1 2) (list 3 4)))

;; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
	    (accumulate-n op init (map cdr seqs)))))

(define seqs1 (list (list 1 2 3)
		    (list 4 5 6)
		    (list 7 8 9)
		    (list 10 11 12)))

;; ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons
		nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map
     (lambda (line) (map (lambda (col) (dot-product line col)) cols))
     m)))

(define v1 '(1 2 3))
(define v2 '(4 5 6))
(define v3 '(7 8 9))
(define m1 `(,v1 ,v2 ,v3))
(define m3 '((1 2) (3 4)))

;; ex 2.38
;; NOTE: transitive functions have the same result for both fold-left and fold-right

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
	  (accumulate op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
	result
	(iter (op result (car rest))
	      (cdr rest))))
  (iter initial sequence))

;; ex 2.39
(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
