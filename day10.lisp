(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (parse-character (complement #'whitespace-char-p)))))

(defun get-map (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (with ret = (make-hash-table :test 'equal))
      (with start-pos = nil)
      (for r from 0)
      (for row in parsed)
      (iter
	(for c from 0)
	(for char in row)
	(when (char= #\S char)
	  (setf start-pos (list r c)))
	(unless (char= #\. char)
	  (setf (gethash (list r c) ret) char)))
      (finally (return (list start-pos ret))))))

;;store offset and position of this cell in relation to neighbour
(defparameter *directions* '(((1 0) :south) ((-1 0) :north)
			     ((0 1) :east) ((0 -1) :west)))

(defparameter *pipe-types* '((#\| (:north :south))
			     (#\- (:east :west))
			     (#\L (:north :east))
			     (#\J (:north :west))
			     (#\7 (:south :west))
			     (#\F (:south :east))
			     (#\S (:north :south :east :west))))

(defun connected? (a b map)
  (let* ((offset (point- b a))
	 (connections
	   (second (find (gethash a map) *pipe-types* :test 'eq :key #'first)))
	 (dir (second (find offset *directions* :test 'equal :key #'first))))
    (and dir (member dir connections))))

(defun both-connected? (a b map)
  (and (connected? a b map) (connected? b a map)))

(defun next-pos (pos map)
  (iter
    (for (offset nil) in *directions*)
    (for next-pos = (point+ offset pos))
    (when (both-connected? pos next-pos map) (collect next-pos))))

(defun day10 (input)
  (destructuring-bind (start-pos map) (get-map input)
    (iter     
      (for (pos from steps root)
	   in-bfs-from start-pos
	   neighbours (lambda (n) (next-pos n map))
	   test 'equal
	   single t)
      (collect pos into loop)
      (maximizing steps)
      (finally (return (list steps loop))))))
