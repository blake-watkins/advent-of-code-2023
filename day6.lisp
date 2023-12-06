(in-package :aoc-2023)

(defun parse-file (part)
  (parse-lines
   (with-monad
     (parse-characters (complement #'digit-char-p))
     (if (= part 1)
         (parse-list (parse-number) (parse-whitespace))
         (with-monad
           (assign number-strings (parse-list (parse-characters #'digit-char-p)
                                              (parse-whitespace)))
           (unit (list (parse-integer (format nil "~{~a~}" number-strings)))))))))

(defun winning-ways (time distance)
  (iter
    (for hold-time from 0 to time)
    (for distance-travelled = (* hold-time (- time hold-time)))
    (counting (> distance-travelled distance))))

(defun day6 (input &key (part 1))
  (let ((parsed (run-parser (parse-file part) input)))
    (reduce #'* (map 'list #'winning-ways (first parsed) (second parsed)))))
