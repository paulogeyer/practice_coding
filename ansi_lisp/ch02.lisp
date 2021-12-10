(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares (i end)
  (if (> i end)
      'done
      (progn
	(format t "~A ~A~%" i (* i i))
	(show-squares (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

(defun our-length (lst)
  (if (null lst)
      0
      (+ (our-length (cdr lst)) 1)))

(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))

(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

;; ex 7, find if list has a list
(defun has-list (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
	  t
	  (has-null (cdr lst)))))

;; ex 8, define recursive and iterative functions
(defun print-dots (n)
  (if (= n 0)
      (format t "~%")
      (progn
	(format t ".")
	(print-dots (- n 1)))))

(defun print-dots (n)
  (do ((i 1 (+ i 1)))
      ((> i n) 'done)
    (format t "."))
  (format t "~%"))

(defun count-obj (lst obj)
  (if (null lst)
      0
      (+ (count-obj (cdr lst) obj)
	 (if (eq (car lst) obj)
	     1
	     0))))

(defun count-obj (lst obj)
  (let ((i 0))
    (dolist (el lst)
      (if (eq el obj)
	  (incf i)))
    i))

;; ex 9, fix functions
(defun summit (lst)
  (apply #'+ (remove nil lst)))

(defun summit (lst)
  (let ((x (car lst)))
    (if (null lst)
	0
	(if (null x)
	    (summit (cdr lst))
	    (+ x (summit (cdr lst)))))))
