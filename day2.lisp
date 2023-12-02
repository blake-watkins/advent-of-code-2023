(in-package :aoc-2023)

(defun parse-roll ()
  (parse-list
   (with-monad
     (assign amount (parse-number))
     (parse-string " ")
     (assign color (parse-keyword #'alphanumericp))
     (unit (list amount color)))
   ", "))

(defun parse-game ()
  (with-monad
    (parse-string "Game ")
    (assign id (parse-number))
    (parse-string ": ")
    (assign rolls (parse-list (parse-roll) "; "))
    (unit (list id rolls))))

(defun game-valid? (limits game)
  (iter
    (for (limit-amount limit-color) in limits)
    (always
     (iter
       (for rolls in (second game))
       (always
        (iter
          (for (roll-amount roll-color) in rolls)
          (never (and (eq roll-color limit-color)
                      (> roll-amount limit-amount)))))))))

(defun game-power (game)
  (let ((maxs (iter
                (with maximums = (make-hash-table :test 'eq))
                (for roll-set in (second game))
                (iter
                  (for (amount color) in roll-set)
                  (setf (gethash color maximums)
                        (max amount (gethash color maximums 0))))
                (finally (return maximums)))))
    (reduce #'* (mapcar (lambda (c) (gethash c maxs 0)) '(:red :blue :green)))))

(defun day2 (input &key (part 1))
  (let ((limits '((12 :red) (13 :green) (14 :blue)))
        (games (run-parser (parse-lines (parse-game)) input)))
    (if (= part 1)
        (iter
          (for game in games)
          (when (game-valid? limits game) (sum (first game))))
        (reduce #'+ (mapcar #'game-power games)))))

