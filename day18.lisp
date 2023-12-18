(in-package :aoc-2023)

(defun parse-hex-code ()
  (with-monad
    (parse-character #\#)
    (assign dist-digits (n-of 5 (parse-digit :base 16)))
    (assign dir-digit (parse-character "0123"))
    (unit (list (case dir-digit (#\0 :r) (#\1 :d) (#\2 :l) (#\3 :u))
                (digits-to-int dist-digits :base 16)))))

(defun parse-file (part)
  (if (= part 1)
      (one-or-more (with-monad
                     (assign dir (parse-keyword "ULRD"))
                     (parse-space)
                     (assign amt (parse-number))
                     (parse-until (parse-newline))
                     (unit (list dir amt))))
      (parse-lines (parse-until (parse-bracketed (parse-hex-code) "()")))))

(defparameter *directions* '((:u (-1 0)) (:r (0 1)) (:d (1 0)) (:l (0 -1)) ))

(defun move (pos dir &optional (amt 1))
  (point+ pos (point* amt (second (assoc dir *directions*)))))

(defun day18 (input &key (part 1))
  (let ((parsed (run-parser (parse-file part) input)))
    (destructuring-bind (corners boundary)
        (iter
          (with cur = '(0 0))
          (with boundary = 0)
          (for (dir amt) in parsed)
          (incf boundary amt)
          (setf cur (move cur dir amt))
          (collect cur into corners)
          (finally (return (list corners boundary))))
      (+ (interior (abs (area corners)) boundary) boundary))))

;; https://en.wikipedia.org/wiki/Shoelace_formula
(defun area (corners)
  (/ (iter
       (for (a b . rest) on corners)
       (when (null b) (setf b (first corners)))
       (summing (- (* (first a) (second b)) (* (first b) (second a)))))
     2))

;; https://en.wikipedia.org/wiki/Pick%27s_theorem
(defun interior (area boundary)
  (- (+ area 1 ) (/ boundary 2)))
