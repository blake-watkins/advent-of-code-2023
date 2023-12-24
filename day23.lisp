(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (parse-character "#.><^v"))))

(defparameter *directions* '((-1 0) (0 1) (1 0) (0 -1)))

(defun neighbours (cur map part)
  (let ((possible-directions
          (if (= part 1)
              (case (gethash cur map)
                (#\. *directions*)
                (#\> `((0 1)))
                (#\< `((0 -1)))
                (#\^ `((-1 0)))
                (#\v `((1 0))))
              *directions*)))
    (remove-if-not
     (lambda (n)
       (let ((char (gethash n map)))
         (and char (not (char= char #\#)))))
     (mapcar (lambda (d) (point+ cur d)) possible-directions))))

(defun find-path-reduced (cur end distance visited graph)
  (if (equal cur end)
      distance
      (let ((neighbours (remove-if
                         (lambda (n) (gethash (first n) visited))
                         (gethash cur graph))))
        (iter
          (initially (setf (gethash cur visited) t))
          (for (n dist) in neighbours)
          (maximizing (find-path-reduced n end (+ dist distance) visited graph) into ret)
          (finally (remhash cur visited)
                   (return (or ret 0)))))))

(defun reduced-neighbours (cur map part)
  (let ((neighbours (neighbours cur map part)))
    (iter
      (for neighbour in neighbours)
      (collect
          (iter
            (for path-square first neighbour then next-path-square)
            (for path-square-prev previous path-square initially cur)
            (for path-square-neighbours = (neighbours path-square map part))
            (for distance from 1)
            
            (while (= 2 (length path-square-neighbours)))
            (for next-path-square =
                 (first (remove path-square-prev
                                path-square-neighbours
                                :test 'equal)))
            (finally (return (list path-square distance))))))))

(defun build-reduced-graph (start map part)
  (iter
    (with to-visit = (make-queue))
    (with graph = (make-hash-table :test 'equal))
    (initially (queue-push-backf start to-visit))
    (until (queue-empty-p to-visit))
    (for cur = (queue-pop-frontf to-visit))
    (unless (gethash cur graph)
      (for neighbours = (reduced-neighbours cur map part))
      (setf (gethash cur graph) neighbours)
      (iter
        (for (neighbour dist) in neighbours)
        (unless (gethash neighbour graph)
          (queue-push-backf neighbour to-visit))))
    (finally (return graph))))

(defun day23 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (start (list 0 (position #\. (first parsed))))
         (end (list (1- (length parsed)) (position #\. (car (last parsed)))))
         (map (hash-table-from-list-list parsed))
         (visited (make-hash-table :test 'equal)))
    (find-path-reduced start end 0 visited (build-reduced-graph start map part))))

