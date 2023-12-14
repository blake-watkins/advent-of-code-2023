(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines
   (one-or-more
    (with-monad
      (assign char (parse-character "O.#"))
      (unit (case char (#\. :blank) (#\# :fixed) (#\O :round)))))))

;; NEXT-ROCK-INDEX tracks the next index in each column that a round rock can go
;; into. Assumes ROCKS is sorted from topmost rocks. :FIXED rocks stay
;; where they are, :ROUND rocks go into the next rock index for the column.
(defun roll-north (rocks)
  (iter
    (with next-rock-index = (make-hash-table))
    (for ((r c) rock-type) in rocks)
    (for this-r = (case rock-type
                    (:fixed r)
                    (:round (gethash c next-rock-index 0))))
    (collect `((,this-r ,c) ,rock-type))
    (setf (gethash c next-rock-index) (1+ this-r))))

(defun north-load (rocks dimension)
  "Calculate load on north wall. "
  (iter
    (for ((r c) rock-type) in rocks)
    (when (eq rock-type :round)
      (sum (- dimension r)))))

(defun topmost-leftmost< (a b)
  "Compares A and B. A is less than B if it's above, or left on the same row. "
  (or (and (= (caar a) (caar b)) (< (cadar a) (cadar b)))
      (< (caar a) (caar b))))

(defun rotate-rocks (rocks dimension)
  "Rotate ROCKS clockwise. Assumes rocks are in a square map. "
  (iter
    (for ((r c) rock-type) in rocks)
    (collect `((,c ,(- dimension 1 r)) ,rock-type) into ret)
    (finally (return (sort ret #'topmost-leftmost<)))))

(defun roll-cycle (rocks dimension &optional (cycles 1))
  "Roll ROCKS north, west, south, then east. Repeats cycle CYCLES times. "
  (iter
    (repeat (* 4 cycles))
    (setf rocks (rotate-rocks (roll-north rocks) dimension))
    (finally (return rocks))))

(defun get-rocks (map)
  (iter outer
    (for row in map)
    (for r from 0)
    (iter
      (for rock-type in row)
      (for c from 0)
      (unless (eq :blank rock-type)
        (in outer (collect `((,r ,c) ,rock-type)))))))

(defun find-nth-load (n rocks dimension)
  (destructuring-bind (cycle-start cycle-end)
      (iter
        (with cache = (make-hash-table :test 'equal))
        (for idx from 0)
        (for iter-rocks initially rocks then (roll-cycle iter-rocks dimension))
        (until (gethash iter-rocks cache))
        (setf (gethash iter-rocks cache) idx)
        (finally (return (list (gethash iter-rocks cache) idx))))
    (let* ((cycle-length (- cycle-end cycle-start))
           (cycles (if (< n cycle-start)
                       n
                       (+ cycle-start (mod (- n cycle-start) cycle-length)))))
      (north-load (roll-cycle rocks dimension cycles) dimension))))

(defun day14 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (rocks (get-rocks parsed))
         (dimension (length parsed)))
    (if (= part 1)
        (north-load (roll-north rocks) dimension)
        (find-nth-load 1000000000 rocks dimension))))
