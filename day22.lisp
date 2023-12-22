(in-package :aoc-2023)

(defun vertical< (a b)
  (< (apply #'min (mapcar #'third a)) (apply #'min (mapcar #'third b))))

(defun xy-squares (brick)
  (let ((mins (mapcar #'min (first brick) (second brick)))
        (maxes (mapcar #'max (first brick) (second brick))))
    (iter outer
      (for x from (first mins) to (first maxes))
      (iter
        (for y from (second mins) to (second maxes))
        (in outer (collect (list x y)))))))

(defun xyz-squares (brick &optional (z-decrease 0))
  (let ((mins (mapcar #'min (first brick) (second brick)))
        (maxes (mapcar #'max (first brick) (second brick))))
    (iter outer
      (for x from (first mins) to (first maxes))
      (iter
        (for y from (second mins) to (second maxes))
        (iter
          (for z from (third mins) to (third maxes))          
          (in outer (collect (list x y (- z z-decrease)))))))))

(defun parse-file ()
  (parse-lines (parse-list (parse-number-list) "~")))

(defun day22 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (sorted (sort parsed #'vertical<))
         (heights (make-hash-table :test 'equal))
         (supports (make-hash-table :test 'equal
                    )))
    (iter
      (for brick in sorted)
      (for current-height =
           (iter
             (for square in (xy-squares brick))
             (maximizing (first (gethash square heights (list 0 nil))))))
      (for height-diff = (- (apply #'min (mapcar #'third brick)) current-height 1))
      (iter
        (for square in (xy-squares brick))
        (for (height supporter) = (gethash square heights nil))
        (when (and height (= height current-height))
          (setf (gethash supporter supports)
                (union (gethash supporter supports nil)
                       (list brick)
                       :test 'equal))))
      (iter
        (for square in (xyz-squares brick height-diff))
        (setf (gethash (list (first square) (second square)) heights)
              (list (third square) brick)))
      ;(format t "~a ~a ~%" heights supports)
      )
    (count-disintegrate supports sorted)))

(defun count-disintegrate (supports bricks)
  (let ((num-supports (make-hash-table :test 'equal)))
    (iter
      (for (supporter supported) in-hashtable supports)
      (iter
        (for supported-brick in supported)
        (incf (gethash supported-brick num-supports 0))))
    (iter
      (for brick in bricks)
      (counting (or (null (gethash brick supports))
                    (every (lambda (s) (> (gethash s num-supports) 1))
                           (gethash brick supports)))))
    (chain-reaction bricks supports num-supports)))
;; can be disintegrated if it's not supporting anything, or if it's not the only
;; brick supporting another brick.

;; count bricks that can't get to the ground except through disintegrated brick
(defun chain-reaction (bricks supports num-supports)
  (let ((roots (iter
                 (for b in bricks)
                 (unless (gethash b num-supports)
                   (collect b)))))
    (iter
      (for brick in bricks)
      (collect
          (- (length bricks)
             (iter
                  (with seen = (make-hash-table :test 'equal))
                  (for (pos parent steps root) in-bfs-from (remove brick roots :test 'equal)
                       neighbours (lambda (b)
                                    (remove brick (gethash b supports) :test 'equal))
                       test 'equal)
                  (setf (gethash pos seen) t)
               (finally (return (hash-table-count seen))))
             1)))))
