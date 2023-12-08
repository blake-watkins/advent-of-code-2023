(in-package :aoc-2023)

(defun parse-file ()
  (with-monad
    (assign directions (one-or-more (parse-character "LR")))
    (n-of 2 (parse-newline))
    (assign network
            (parse-lines
             (with-monad
               (assign name (parse-keyword))
               (parse-string " = ")
               (assign paths (parse-bracketed
                              (parse-list (parse-keyword #'alphanumericp) ", ")
                              "()"))
               (unit (cons name paths)))))
    (unit (list directions network))))

(defun make-circular (lst)
  (setf (cdr (last lst)) lst))

(defun locations-ending-with (network char)
  (iter 
    (for (name nil nil) in network)
    (when (char= char (elt (symbol-name name) 2)) (collect name))))

(defun steps-to-finish (directions network start-symbol end-symbols)
  (iter
    (for steps from 0)
    (for direction in directions)
    (for location initially start-symbol then next-location)
    (until (member location end-symbols))
    (for next-locations = (find location network :key #'first))
    (for next-location = (if (char= direction #\L)
                             (second next-locations)
                             (third next-locations)))
    (finally (return steps))))

(defun day8 (input &key (part 1))
  (destructuring-bind (directions network) (run-parser (parse-file) input)
    (setf directions (make-circular directions))
    (if (= part 1)
        (steps-to-finish directions network :aaa '(:zzz))
        (apply #'lcm
               (iter
                 (with end-symbols = (locations-ending-with network #\Z))
                 (for start-symbol in (locations-ending-with network #\A))
                 (collect
                     (steps-to-finish directions network
                                      start-symbol end-symbols)))))))
