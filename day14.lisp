(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (one-or-more (bind (parse-character "O.#")
                                  (lambda (c)
                                    (unit
                                     (case c
                                       (#\. :blank)
                                       (#\# :fixed)
                                       (#\O :round))))))))

(defun roll-north (rocks map)
  (iter
    (for c below (length (first map)))
    (appending
     (iter
       (with rock-idx = 0)
       (for row in map)
       (for r from 0)
       (cond
         ((eq (elt (elt map r) c) :fixed) (setf rock-idx (1+ r)))
         ((member (list r c) rocks :test 'equal)
          (progn
            (collect (list rock-idx c))
            (incf rock-idx))))))))

(defun roll-south (rocks map)
  (iter
    (for c below (length (first map)))
    (appending
     (iter
       (with rock-idx = (1- (length map)))
       (for r from (1- (length map)) downto 0)
       (cond
         ((eq (elt (elt map r) c) :fixed) (setf rock-idx (1- r)))
         ((member (list r c) rocks :test 'equal)
          (progn
            (collect (list rock-idx c))
            (decf rock-idx))))))))

(defun roll-west (rocks map)
  (iter
    (for r below (length map))
    (appending
     (iter
       (with rock-idx = 0)
       (for c from 0 below (length (first map)))
       (cond
         ((eq (elt (elt map r) c) :fixed) (setf rock-idx (1+ c)))
         ((member (list r c) rocks :test 'equal)
          (progn
            (collect (list r rock-idx))
            (incf rock-idx))))))))

(defun roll-east (rocks map)
  (iter
    (for r below (length map))
    (appending
     (iter
       (with rock-idx = (1- (length (first map))))
       (for c from (1- (length (first map))) downto 0)
       (cond
         ((eq (elt (elt map r) c) :fixed) (setf rock-idx (1- c)))
         ((member (list r c) rocks :test 'equal)
          (progn
            (collect (list r rock-idx))
            (decf rock-idx))))))))

(defun north-load (rocks map)
  (iter
    (for (r c) in rocks)
    (sum (- (length map) r))))

(defun day14 (input)
  (let* ((parsed (run-parser (parse-file) input))
        (rocks (iter
                 (for row in parsed)
                 (for r from 0)
                 (appending (iter
                              (for square in row)
                              (for c from 0)
                              (when (eq square :round) (collect (list r c))))))))
    (iter
      (repeat 200)
      (setf rocks (roll-north rocks parsed))
      (setf rocks (roll-west rocks parsed))
      (setf rocks (roll-south rocks parsed))
      (setf rocks (roll-east rocks parsed))
      (collect (north-load rocks parsed)))))
