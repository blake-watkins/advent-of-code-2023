(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (parse-character ".#"))))

(defun galaxies (map)
  (iter outer
    (for row in map)
    (for r from 0)
    (iter
      (for char in row)
      (for c from 0)
      (when (char= #\# char) (in outer (collect (list r c)))))))

(defun blank-rows (map)
  (iter
    (for row in map)
    (for r from 0)
    (when (every (lambda (c) (char= #\. c)) row) (collect r))))

(defun blank-cols (map)
  (iter
    (for c from 0 below (length (first map)))
    (when (every (lambda (row) (char= #\. (elt row c))) map) (collect c))))

(defun included-blanks (start end blanks)
  (count-if (lambda (blank) (<= (min start end) blank (max start end))) blanks))

(defun galaxy-distance (a b blanks expansion-factor)
  (flet ((coord-distance (a b blanks)
	   (+ (abs (- b a))
	      (* (1- expansion-factor) (included-blanks a b blanks)))))
    (reduce #'+ (map 'list #'coord-distance a b blanks))))

(defun day11 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
	 (galaxies (galaxies parsed))
	 (blanks (list (blank-rows parsed) (blank-cols parsed)))
	 (expansion (if (= part 1) 2 1000000)))
    (reduce #'+ (mapcar (lambda (pair)
			  (destructuring-bind (a b) pair
			    (galaxy-distance a b blanks expansion)))
			(pairs galaxies)))))
