;; first part

(setq *FILE* #P"/home/paulo/code/practice_coding/aoc/2021/day1.txt")

(with-open-file (s *FILE* :direction :input)
  (let ((count 0)
	(prev nil))
    (do ((l (read-line s) (read-line s nil 'eof)))
	((eq l 'eof) "Reached end of file.")
      (let ((n (parse-integer l)))
	(if (and prev (< prev n))
	    (incf count))
	(setf prev n)))
    (format t "total: ~A~%" count)))

;; second part

(with-open-file (s *FILE* :direction :input)
  (let ((count 0)
	(prev nil)
	(windows (make-array 0 :adjustable t)))
    
