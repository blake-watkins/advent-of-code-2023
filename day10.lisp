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
	(setf (gethash (list r c) ret) char))
      (finally (return (list start-pos ret))))))

;;store offset and position of this cell in relation to neighbour
(defparameter *directions* '(((-1 0) :north)
			     ((0 1) :east)
			     ((1 0) :south)
			     ((0 -1) :west)))

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

(defun mark-loop (loop-squares)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for square in loop-squares)
    (setf (gethash square ret) :loop)
    (finally (return ret))))

(defun mark-connected (from mark marked map)
  (if (or (gethash from marked) (not (gethash from map)))
      marked
      (iter
	(for vertex-info
	     in-bfs-from from
	     neighbours (lambda (from)
			  (iter
			    (for (offset nil) in *directions*)
			    (for next-square = (point+ from offset))
			    (when (and (gethash next-square map)
				       (not (gethash next-square marked)))
			      (collect next-square))))
	     test 'equal
	     single t)
	(setf (gethash (first vertex-info) marked) mark)
	(finally (return marked)))))

(defun turn (pos dir ccw)
  (destructuring-bind (offset offset-dir)
      (elt *directions* (mod (+ (position dir *directions* :key #'second)
				(ecase ccw (:cw 1) (:ccw -1)))
			     (length *directions*)))
    (list (point+ pos offset) offset-dir)))

(defun mark-connected-from-direction (pos dir marked map)
  (mark-connected (first (turn pos dir :cw)) :a marked map)
  (mark-connected (first (turn pos dir :ccw)) :b marked map))

(defun walk-loop (start-pos marked map)
  (iter
    (for prev-pos previous pos)
    (for pos initially start-pos then next-pos)
    (for next-pos = (first (remove prev-pos (next-pos pos map) :test 'equal)))
    (for dir =
	 (second (find (point- next-pos pos) *directions* :test 'equal :key #'first)))
    (for prev-dir previous dir)
    (when (and prev-dir (not (eq prev-dir dir)))
      (mark-connected-from-direction pos prev-dir marked map))
    (mark-connected-from-direction pos dir marked map)
    (until (and prev-pos (equal pos start-pos)))
    (finally (return marked))))

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
      (finally (return (list steps (hash-table-count map) (count-table (walk-loop start-pos (mark-loop loop) map) )))))))

(defun count-table (marked)
  (iter
    (with ret = (make-hash-table))
    (for (key val) in-hashtable marked)
    (incf (gethash val ret 0))
    (finally (return ret))))
