(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines
   (with-monad
     (parse-string "Card")
     (parse-whitespace)
     (assign id (parse-number))
     (parse-string ":")
     (parse-whitespace)
     (assign winning-numbers (parse-list (parse-number) (parse-whitespace)))
     (parse-string " |")
     (parse-whitespace)
     (assign my-numbers (parse-list (parse-number) (parse-whitespace)))
     (unit (list id winning-numbers my-numbers)))))

(defun winning-count (card)
  (iter
    (for number in (third card))
    (counting (find number (second card)))))

(defun card-score (card)
  (let ((winning-count (winning-count card)))
    (if (> winning-count 0) (expt 2 (1- winning-count)) 0)))

(defun day4 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input)))
    (if (= part 1)
        (reduce #'+ (mapcar #'card-score parsed))
        (iter
          (with repeats = (make-hash-table :test 'eq))
          (for card in parsed)
          (iter
            (for i from 1)
            (repeat (winning-count card))
            (incf (gethash (+ (first card) i) repeats 1)
                  (gethash (first card) repeats 1)))
          (sum (gethash (first card) repeats 1))))))
