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
(defparameter *directions*
  '(((-1 0) :north) ((0 1) :east) ((1 0) :south) ((0 -1) :west)))

(defparameter *pipe-types* '((#\| (:north :south))
			     (#\- (:east :west))
			     (#\L (:north :east))
			     (#\J (:north :west))
			     (#\7 (:south :west))
			     (#\F (:south :east))
			     (#\S (:north :south :east :west))))

(defun pipe-connected? (a b map)
  "Are A and B both correctly pointing at each other to make a pipe?"
  (flet ((connected-one-way? (a b map)
	   (let* ((offset (point- b a))
		  (dir (find offset *directions* :test 'equal :key #'first))
		  (dir-name (second dir))
		  (map-char (gethash a map))
		  (pipe-type (find map-char *pipe-types* :test 'eq :key #'first))
		  (connection-names (second pipe-type)))
	     (and dir-name (member dir-name connection-names)))))
    (and (connected-one-way? a b map) (connected-one-way? b a map))))

(defun pipe-neighbours (pos map)
  "Return list of (POS DIR) pairs that are correctly connected neighbours of POS."
  (iter
    (for (offset dir) in *directions*)
    (for next-pos = (point+ offset pos))
    (when (pipe-connected? pos next-pos map) (collect (list next-pos dir)))))

(defun mark-loop (loop-squares)
  "Return hash table with all squares in LOOP-SQUARES marked as :LOOP."
  (iter
    (with ret = (make-hash-table :test 'equal))
    (for square in loop-squares)
    (setf (gethash square ret) :loop)
    (finally (return ret))))

(define-condition outside-map (condition) ())

(defun can-mark? (pos marked map)
  "Is POS on the map and unmarked? Signal 'OUTSIDE-MAP condition if outside map."
  (when (not (gethash pos map)) (signal 'outside-map))
  (and (gethash pos map) (not (gethash pos marked))))

(defun unmarked-neighbours (pos marked map)
  "Return list of neighbours that can be marked. "
  (iter
    (for (offset nil) in *directions*)
    (for next-square = (point+ pos offset))
    (when (can-mark? next-square marked map) (collect next-square))))

(defun mark-connected (from mark marked map)
  "Mark all connected neighbours of FROM with MARK. Alters MARKED hash table."
  (when (can-mark? from marked map)
    (iter
      (for (pos . nil)
	   in-bfs-from from
	   neighbours (lambda (from) (unmarked-neighbours from marked map))
	   test 'equal
	   single t)
      (setf (gethash pos marked) mark))))

(defun turn (dir rotation)
  "Return element of *DIRECTIONS* list corresponding to DIR (:north etc) rotated by ROTATION (:cw, :ccw etc). "
  (elt *directions*
       (mod (+ (position dir *directions* :key #'second)
	       (ecase rotation (:cw 1) (:ccw -1)))
	    (length *directions*))))

(defparameter *outside-mark* nil)

(defun mark-pos-sides (pos dir marked map)
  "Mark sides of pos with different marks. Alters MARKED hash table. Handles 'OUTSIDE-MAP condition and sets *OUTSIDE-MARK* if any landed outside the map."
  (iter
    (for rotation in '(:cw :ccw))
    (handler-case (mark-connected (point+ pos (first (turn dir rotation)))
				  rotation marked map)
      (outside-map () (setf *outside-mark* rotation)))))

(defun walk-loop (start-pos marked map)
  (iter
    (for prev-pos previous pos)
    (for pos initially start-pos then next-pos)
    (for neighbours =
	 (remove prev-pos (pipe-neighbours pos map) :test 'equal :key #'first))
    (for (next-pos dir) = (first neighbours))
    (for prev-dir previous dir)
    (mapc (lambda (dir) (mark-pos-sides pos dir marked map))
	  (remove nil (list dir prev-dir)))
    (until (and prev-pos (equal pos start-pos)))))

(defun count-table (marked)
  (iter
    (with ret = (make-hash-table))
    (for (key val) in-hashtable marked)
    (incf (gethash val ret 0))
    (finally (return ret))))

(defun day10 (input &key (part 1))
  (destructuring-bind (start-pos map) (get-map input)
    (destructuring-bind (steps loop-squares)
	(iter     
	  (for (pos from steps root)
	       in-bfs-from start-pos
	       neighbours (lambda (n) (mapcar #'first (pipe-neighbours n map)))
	       test 'equal
	       single t)
	  (collect pos into loop)
	  (maximizing steps)
	  (finally (return (list steps loop))))
      (if (= part 1)
	  steps
	  (let ((marked (mark-loop loop-squares)))
	    (walk-loop start-pos marked map)
	    (gethash (if (eq *outside-mark* :cw) :ccw :cw)
		     (count-table marked)))))))


