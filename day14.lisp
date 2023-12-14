(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines
   (one-or-more
    (with-monad
      (assign char (parse-character "O.#"))
      (unit (case char (#\. :blank) (#\# :fixed) (#\O :round)))))))

(defun roll-north (rocks)
  (iter
    (with next-rock-index = (make-hash-table))
    (for ((r c) rock-type) in rocks)
    (case rock-type
      (:fixed
       (collect (list (list r c) :fixed))
       (setf (gethash c next-rock-index 0) (1+ r)))
      (:round
       (collect (list (list (gethash c next-rock-index 0) c) :round))
       (incf (gethash c next-rock-index 0))))))

(defun north-load (rocks map-rows)
  (iter
    (for ((r c) rock-type) in rocks)
    (when (eq rock-type :round)
      (sum (- map-rows r)))))

(defun rotate-rocks (rocks map-rows)
  (iter
    (for ((r c) rock-type) in rocks)
    (collect `((,c ,(- map-rows 1 r)) ,rock-type) into ret)
    (finally (return (sort ret #'topmost-leftmost<)))))

(defun topmost-leftmost< (a b)
  (or (and (= (caar a) (caar b)) (< (cadar a) (cadar b)))
      (< (caar a) (caar b))))

(defun cycle (rocks map-rows)
  (iter
    (repeat 4)
    (setf rocks (roll-north rocks))
    (setf rocks (rotate-rocks rocks map-rows))
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

(defun day14 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input))
         (rocks (get-rocks parsed))
         (map-rows (length parsed)))
    (if (= part 1)
        (north-load (roll-north rocks) map-rows)
        (iter
          (repeat 200)
          (setf rocks (cycle rocks map-rows))
          (collect (north-load rocks map-rows))))))
