(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (parse-digit))))

(defparameter *directions* '((-1 0) (0 1) (1 0) (0 -1) ))

(defun turn (dir c-cw)
  (elt *directions*
       (mod (+ (position dir *directions* :test 'equal) (if (eq c-cw :cw) 1 -1))
            (length *directions*))))

(defun neighbours (pos dir straight map)
  (let ((dirs (if (>= straight 4)
                  `((,(turn dir :cw) 1) (,(turn dir :ccw) 1))
                  ())))
    (when (< straight 10) (push (list dir (1+ straight)) dirs))
    (remove-if-not
     (lambda (pos) (gethash pos map))
     (mapcar (lambda (info) `((,(point+ pos (first info))
                               ,(first info)
                               ,(second info))
                              ,(gethash (point+ pos (first info)) map)))
             dirs)
     :key #'caar)))


(defun day17 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (map (hash-table-from-list-list parsed)))
    (iter
      (with end = (hash-table-dimensions map))
      (with parents = (make-hash-table :test 'equal))
      (for (vertex parent distance) in-dijkstra-from '((0 0) (1 0) 0)
           :neighbours (lambda (info)
                         (neighbours (first info) (second info) (third info) map)))
      (unless (gethash (first vertex) parents)
        (setf (gethash (first vertex) parents) (first parent)))
      (until (and (equal end (first vertex))
                  (>= (third vertex) 4)))
      (finally (return distance)))))
