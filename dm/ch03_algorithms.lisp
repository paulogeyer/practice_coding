(defun lst-max (lst)
  "get max element from list"
  (let ((max (first lst)))
    (loop for element in lst do
	 (if (> element max)
	     (setq max element)))
    max))

(defun binary-search (n lst)
  (let ((i 1)
	(j (length lst)))
    (do ()
	((>= i j))
      (setq m (floor (/ (+ i j) 2)))
      (if (> n (nth m lst))
	  (setq i (1+ m))
	  (setq j m)))
    (if (= n (nth i lst))
	i
	0)))

(defun bubblesort (lst)
  (let ((lst (copy-seq lst))
	(lst-length (1- (length lst))))
    (loop for i from 0 upto (1- lst-length) do
	 (progn
	   (loop for j from 0 upto (- lst-length i) do
		(if (and (nth j lst) (nth (1+ j) lst)
			 (> (nth j lst) (nth (1+ j) lst)))
		    (rotatef (nth j lst) (nth (1+ j) lst))
		    ))))
    lst))

(defun insertion-sort (lst)
  (setq lst (copy-seq lst))
  (loop for j from 1 upto (1- (length lst)) do
       (progn
	 (setq i 0)
	 (do ()
	     ((<= (nth j lst) (nth i lst)))
	   (incf i))
	 (setf m (nth j lst))
	 (loop for k from 0 upto (- j i 1)
	    do (setf (nth (- j k) lst) (nth (- j k 1) lst)))
	 (setf (nth i lst) m)))
  lst)

(defun change (n)
  "greedy algorithm for finding the change for n cents"
  (let ((coins '(25 10 5 1))
	(change-coins '()))
    (loop for i from 0 upto (1- (length coins)) do
	 (progn 
	   (do ()
	       ((< n (nth i coins)))
	     (push (nth i coins) change-coins)
	     (setq n (- n (nth i coins))))))
    (reverse change-coins)))
