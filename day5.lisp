(in-package :aoc-2023)

(defun parse-map ()
  (with-monad
    (parse-keyword #'alphanumericp)
    (parse-string "-to-")
    (parse-keyword #'alphanumericp)
    (parse-string " map:")
    (parse-newline)
    (assign ranges (parse-lines (parse-list (parse-number) (parse-space))))
    (parse-newline)
    (parse-newline)
    (unit ranges)))

(defun parse-file ()
  (with-monad
    (parse-string "seeds: ")
    (assign seeds (parse-number-list #\ ))
    (parse-newline)
    (parse-newline)
    (assign maps (parse-list (parse-map) ""))
    (unit (list seeds maps))))

(defun containing-range (value ranges)
  "Return the first range in RANGES that contains VALUE, NIL if none do."
  (some (lambda (range)
	  (let ((src (second range))
		(length (third range)))
	    (when (<= src value (+ src length -1)) range)))
	ranges))

(defun next-range (value ranges)
  "Return range in RANGES with minimum starting pos at or after VALUE, or NIL. "
  (iter
    (for range in ranges)
    (when (>= (second range) value)
      (finding range minimizing (second range)))))

(defun map-interval (interval ranges)
  "Split INTERVAL, map to new intervals based on RANGES. Returns intervals list."
  (destructuring-bind (start length) interval
    (if (= 0 length)
	'()
	(let ((containing-range (containing-range start ranges)))
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

(defun map-intervals (intervals all-ranges)
  (reduce (lambda (intervals ranges)
	    (iter
	      (for interval in intervals)
	      (appending (map-interval interval ranges))))
	  all-ranges
	  :initial-value intervals))

(defun day5 (input &key (part 1))
  (destructuring-bind (seeds ranges) (run-parser (parse-file) input)
    (let ((intervals (if (= part 1)
			 (mapcar (lambda (seed) `(,seed 1)) seeds)
			 (iter
			   (for (a b) on seeds by #'cddr)
			   (collect (list a b))))))
      (reduce #'min (map-intervals intervals ranges) :key #'first))))
