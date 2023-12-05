(in-package :aoc-2023)

(defun parse-ranges ()
  (parse-lines
   (parse-list (parse-number) (parse-space))))

(defun parse-map ()
  (with-monad
    (assign source (parse-keyword #'alphanumericp))
    (parse-string "-to-")
    (assign destination (parse-keyword #'alphanumericp))
    (parse-string " map:")
    (parse-newline)
    (assign ranges (parse-ranges))
    (parse-newline)
    (parse-newline)
    (unit (list source destination ranges))))

(defun parse-file ()
  (with-monad
    (parse-string "seeds: ")
    (assign seeds (parse-number-list #\ ))
    (parse-newline)
    (parse-newline)
    (assign maps (parse-list (parse-map) ""))
    (unit (list seeds maps))))

(defun map-value (value ranges)
  (let ((new-value (iter
		     (for (dest source length) in ranges)
		     (finding (+ dest (- value source)) such-that
			      (<= source value (+ source length -1))))))
    (if new-value new-value value)))

(defun find-location (value type maps)
  (if (eq :location type)
      value
      (iter
	(for (source dest ranges) in maps)	
	(for mapped-value = (map-value value ranges))
	(finding (find-location mapped-value dest maps) such-that (eq source type)))))


(defun find-interval-location (intervals type maps)
;;  (format t "~a ~a ~a~%" intervals type maps)
  (if (eq :location type)
      intervals
      (let* ((type-map (iter
			 (for type-map in maps)
			 (finding type-map such-that (eq type (first type-map)))))
	     (ranges (third type-map))
	     (new-intervals (iter
			      (for interval in intervals)
			      (appending (map-interval interval ranges)))))
	(find-interval-location new-intervals (second type-map) maps))))

(defun contained-in-range (value ranges)
  (iter
    (for range in ranges)
    (for (dest src length) = range)
    (finding range such-that (<= src value (+ src length -1)))))

(defun next-range (value ranges)
  (iter
    (for range in ranges)
    (for (dest src length) = range)
    (when (>= src value)
      (finding range minimizing src))))

;; (0 7) ((0 5 5) (5 0 5)) -> 5->0 6->1 7->2 0->5 1-> 
(defun map-interval (interval ranges)
;;  (format t "~a~%" interval)
;;  (break)
  (destructuring-bind (start length) interval
    (if (= 0 length)
	nil
	(let ((containing-range (contained-in-range start ranges)))
	  (if containing-range
	      (destructuring-bind (dest src range-length) containing-range
		(let ((new-length (min length (- range-length (- start src)))))
		  (cons (list (+ dest (- start src)) new-length)
			(map-interval (list (+ start new-length)
					    (- length new-length))
				      ranges))))
	      (let ((next-range (next-range start ranges)))
		(if next-range
		    (destructuring-bind (dest src range-length) next-range
		      (declare (ignore dest range-length))
		      (let ((new-length (min length (- src start))))
			(cons (list start new-length)
			      (map-interval (list (+ start new-length)
						  (- length new-length))
					    ranges))))
		    (list (list start length)))))))))
(defun day5 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for seed in (first parsed))
      (minimizing (find-location seed :seed (second parsed))))))

(defun day5-2 (input)
  (let* ((parsed (run-parser (parse-file) input))
	 (intervals (iter
		      (for (a b) on (first parsed) by #'cddr)
		      (collect (list a b)))))
    (find-interval-location intervals :seed (second parsed))))
