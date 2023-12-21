(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (parse-list (parse-character ".#S") "")))

(defparameter *directions* '((1 0) (-1 0) (0 1) (0 -1)))

(defun get-neighbours (square map)
  (iter
    (for direction in *directions*)
    (for neighbour = (point+ square direction))
    (for neighbour-char = (gethash neighbour map))
    (when (and neighbour-char (member neighbour-char '(#\. #\S)))
      (collect neighbour))))

(defun day21 (input)
  (let* ((map (hash-table-from-list-list (run-parser (parse-file) input)))
         (start (iter
                 (for (k v) in-hashtable map)
                 (finding k such-that (char= v #\S))))
        (start-squares (make-hash-table :test 'equal)))
    (setf (gethash start start-squares) t)
    (iter
      (repeat 64)
      (for squares first start-squares then next-squares)
      (for next-squares = (make-hash-table :test 'equal))
      (iter
        (for (square nil) in-hashtable squares)
        (for neighbours = (get-neighbours square map))
        (iter
          (for neighbour in neighbours)
          (setf (gethash neighbour next-squares) t)))
      (finally (return (hash-table-count next-squares))))))
