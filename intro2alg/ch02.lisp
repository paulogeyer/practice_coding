;; insertion sort, pg 19

(defun insertion-sort (lst1)
  (let ((lst (copy-seq lst1)))
    (loop for j from 1 upto (1- (length lst)) do              ;; c1, n
	 (setq key (aref lst j))                              ;; c2, n-1
	 (setq i (1- j))                                      ;; c4, n-1
	 (loop while (and (>= i 0) (> (aref lst i) key)) do   ;; c5, sum t[j] from j=2 upto n
	      (setf (aref lst (1+ i)) (aref lst i))           ;; c6, sum (t[j] - 1) from j=2 upto n
	      (decf i))                                       ;; c7, sum (t[j] - 1) from j=2 upto n
	 (setf (aref lst (1+ i)) key))                        ;; c8, n-1
    lst))

;; T(n) = c1*n + c2*(n-1) + c4*(n-1) + c5*(sum t[j] from j=2 upto n) + c6*(sum t[j]-1 from j=2 upto n)
;;        + c7*(sum t[j]-1 from j=2 upto n) + c8*(n-1)

;; (setq lst2 (make-array 6 :initial-contents '(23 9 11 83 33 42)))

;; (setq lst1 (make-array 6 :initial-contents '(5 2 4 6 1 3)))

;; selection sort, ex 2.2-2, pg 29
(defun selection-sort (lst1)
  (let ((lst (copy-seq lst1)))
    (loop for i from 0 upto (1- (length lst)) do
	 (setq cur-item (aref lst i))
	 (setq min-val cur-item
	       cur-min-index i)
	 (loop for j from (1+ i) upto (1- (length lst)) do
	      (if (< (aref lst j) min-val)
		  (setq min-val (aref lst j)
			cur-min-index j)))
	      ;; (format t "i: ~A~%" i)
	      ;; (format t "j: ~A~%" j))
	 (rotatef (aref lst i) (aref lst cur-min-index)))
    lst))
