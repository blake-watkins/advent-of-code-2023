(in-package :aoc-2023)

(defun get-number-strings (&key part)
  (iter
    (for i from 1 to 9)
    (collect (list (format nil "~a" i) i))
    (when (= part 2)
      (collect (list (format nil "~R" i) i)))))

(defun match-number-string (input number-strings)
  (iter
    (for (str val) in number-strings)
    (for first-match = (search str input))
    (for last-match = (search str input :from-end t))
    (when first-match (finding val minimizing first-match into first-number))
    (when last-match (finding val maximizing last-match into last-number))    
    (finally (return (+ (* 10 first-number) last-number)))))

(defun day1 (input &key (part 1))
  (let ((lines (str:lines input))
	(number-strings (get-number-strings :part part)))    
    (reduce #'+
	    (mapcar (lambda (x) (match-number-string x number-strings)) lines))))
