(in-package :aoc-2023)

(defun parse-color ()
  (parse-keyword #'alphanumericp))

(defun parse-cube-set ()
  (parse-list
   (with-monad
     (assign amount (parse-number))
     (parse-string " ")
     (assign color (parse-color))
     (unit (list amount color)))
   ", "))

(defun parse-game ()
  (with-monad
    (parse-string "Game ")
    (assign id (parse-number))
    (parse-string ": ")
    (assign sets (parse-list (parse-cube-set) "; "))
    (unit (list id sets))))

(defun test-game (cubes game)
  (iter
    (for (amount color) in cubes)
    (always (iter
              (for roll-set in (second game))
              (always (iter
                        (for roll in roll-set)
                        (never (and (eq (second roll) color)
                                    (> (first roll) amount)))))))))

(defun day2 (input)
  (let ((games (run-parser (parse-lines (parse-game)) input)))
    (iter
      (for game in games)
      (when (test-game '((12 :red) (13 :green) (14 :blue)) game)
        (sum (first game))))))

(defun game-power (cubes)
  (reduce #'*
          (mapcar (lambda (color) (gethash color cubes 0)) '(:red :blue :green))))

(defun day2-2 (input)
  (let ((games (run-parser (parse-lines (parse-game)) input)))
    (reduce #'+
            (iter
              (for game in games)
              (collect
                  (iter
                    (with maximums = (make-hash-table :test 'eq))
                    (for roll-set in (second game))
                    (iter
                      (for (amount color) in roll-set)
                      (setf (gethash color maximums)
                            (max amount (gethash color maximums 0))))
                    (finally (return (game-power maximums)))))))))
