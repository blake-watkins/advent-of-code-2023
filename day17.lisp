(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (parse-digit))))

(defparameter *directions* '((-1 0) (0 1) (1 0) (0 -1) ))

(defclass vertex ()
  ((pos :initarg :pos :reader vertex-pos)
   (dir :initarg :dir)
   (straight :initarg :straight :reader vertex-straight)))

(defmethod print-object ((object vertex) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (pos dir straight) object
      (format stream "(~a ~a ~a)" pos dir straight))))

(defmethod fset:compare ((v1 vertex) (v2 vertex))
  (fset:compare-slots v1 v2 'pos 'dir 'straight))

(defun turn (dir c-cw)
  (elt *directions*
       (mod (+ (position dir *directions* :test 'equal) (if (eq c-cw :cw) 1 -1))
            (length *directions*))))

(defun get-next-vertices (vertex part)
  (with-slots (pos dir straight) vertex
    (let ((new-dirs
	    (let ((turn-dirs (mapcar (lambda (d) `(,(turn dir d) 1)) '(:cw :ccw)))
		  (straight-dir `((,dir ,(1+ straight)))))
	      (if (= part 1)
		  (append turn-dirs (when (< straight 3) straight-dir))
		  (append (when (>= straight 4) turn-dirs)
			  (when (< straight 10) straight-dir))))))
      (iter
	(for (new-dir new-straight) in new-dirs)
	(collect (make-instance 'vertex
				:pos (point+ pos new-dir)
				:dir new-dir
				:straight new-straight))))))

(defun neighbours (vertex map part)
  (iter
    (for neighbour in (get-next-vertices vertex part))
    (for cost = (gethash (vertex-pos neighbour) map))
    (when cost (collect (list neighbour cost)))))

(defun day17 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (map (hash-table-from-list-list parsed)))
    (iter
      (for start-dir in '((0 1) (1 0)))
      (minimize
       (iter
	 (with end = (hash-table-dimensions map))
	 (for (vertex parent distance)
              in-dijkstra-from
              (make-instance 'vertex :pos '(0 0) :dir start-dir :straight 1)
              :neighbours (lambda (vertex) (neighbours vertex map part)))
	 (setf (gethash vertex parents) parent)
	 (until (and (equal end (vertex-pos vertex))
                     (or (= part 1) (>= (vertex-straight vertex) 4))))
	 (finally (return distance )))))))
