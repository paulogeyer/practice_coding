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

