(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (parse-character ".#/\\|-"))))

(defparameter *directions*
  '((:north (-1 0)) (:east (0 1)) (:south (1 0)) (:west (0 -1))))

(defun next-square (pos dir)
  (list (point+ pos (second (assoc dir *directions*))) dir))

(defun turn (dir c-cw)
  (first (elt *directions*
              (mod (+ (position dir *directions* :key #'first)
                      (if (eq c-cw :cw) 1 -1))
                   (length *directions*)))))

(defun reflect (dir pos-neg)
  (if (char= pos-neg #\/)
      (case dir (:east :north) (:north :east) (:west :south) (:south :west))
      (case dir (:east :south) (:north :west) (:west :north) (:south :east))))

(defun split (dir vert-horiz)
  (if (char= vert-horiz #\|)
      (case dir ((:east :west) '(:north :south)) (t (list dir)))
      (case dir ((:north :south) '(:east :west)) (t (list dir)))))

(defun next-positions (pos dir map)
  (let ((positions (case (gethash pos map)
		     (#\.
		      (list (next-square pos dir)))
		     ((#\/ #\\)
		      (list (next-square pos (reflect dir (gethash pos map)))))
		     ((#\| #\-)
		      (mapcar (lambda (dir) (next-square pos dir))
			      (split dir (gethash pos map)))))))
    (remove-if-not (lambda (pos) (gethash pos map))
		   positions
		   :key #'first)))

(defun count-energized (start-pos start-dir map)
  (iter
    (with visited = (make-hash-table :test 'equal))
    (for ((pos dir) from steps root) in-bfs-from (list start-pos start-dir)
         :neighbours (lambda (n) (next-positions (first n) (second n) map))
         :test 'equal
         :single t)
    (setf (gethash pos visited) t)
    (finally (return (hash-table-count visited)))))

(defun get-start-squares (dimensions)
  (destructuring-bind (max-r max-c) dimensions
    (concatenate
     'list
     (iter
       (for c to max-c)
       (collect `((0 ,c) :south))
       (collect `((,max-r ,c) :north)))
     (iter
       (for r to max-r)
       (collect `((,r 0) :east))
       (collect `((,r ,max-c) :west))))))

(defun day16 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (map (hash-table-from-list-list parsed)))
    (if (= part 1)
	(count-energized '(0 0) :east map)
	(iter
	  (for (pos dir) in (get-start-squares (hash-table-dimensions map)))
	  (maximizing (count-energized pos dir map))))))
