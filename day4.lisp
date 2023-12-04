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
    (with (nil winning-numbers my-numbers) = card)
    (for number in my-numbers)
    (count (find number winning-numbers))))

(defun card-score (card)
  (let ((winning-count (winning-count card)))
    (if (> winning-count 0) (expt 2 (1- winning-count)) 0)))

(defun day4 (input &key (part 1))
  (let* ((cards (run-parser (parse-file) input)))
    (if (= part 1)
        (reduce #'+ (mapcar #'card-score cards))
        (iter
          (with instances = (make-hash-table))
          (for card in cards)
          (for card-num = (first card))
          (for num-instances = (gethash card-num instances 1))
          (sum num-instances)
          (iter
            (repeat (winning-count card))
            (for i from (1+ card-num))
            (incf (gethash i instances 1) num-instances))))))
