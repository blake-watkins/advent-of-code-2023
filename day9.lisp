(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines (parse-number-list #\Space)))

(defun get-differences (values)
  (iter
    (for difference initially values then
         (iter
           (for (a b) on difference)
           (when b (collect (- b a)))))
    (collect difference)
    (until (every #'zerop difference))))

(defun predict (values part)
  (reduce (lambda (a diffs)
            (if (= part 1)
                (+ a (car (last diffs)))
                (- (first diffs) a)))
          (reverse (get-differences values))
          :initial-value 0))

(defun day9 (input &key (part 1))
  (reduce #'+
          (run-parser (parse-file) input)
          :key (lambda (values) (predict values part))))
