(in-package :aoc-2023)

(defun parse-file ()
  (one-or-more
   (with-monad
     (assign ret (parse-lines (one-or-more (parse-character "#."))))
     (parse-newline)
     (parse-newline)
     (unit ret))))

(defun find-vertical-reflection (map)
  (iter
    (for reflect from 1 below (length (first map)))
    (when
        (every
         (lambda (row)
           (iter
             (for i from 0 below (length row))
             (for char-a = i)
             (for char-b = (+ (- reflect (/ 1 2)) (- (- reflect (/ 1 2)) char-a)))
             (always (or (not (<= 0 char-b (1- (length row))))
                         (char= (elt row char-a) (elt row char-b))))))
         map)
      (collect reflect))))

(defun find-horizontal-reflection (map)
  (iter
    (for reflect from 1 below (length map))
    (when (iter
            (for i from 0 below (length map))
            (for row-a = i)
            (for row-b = (+ (- reflect (/ 1 2)) (- (- reflect (/ 1 2)) row-a)))
            (always (or (not (<= 0 row-b (1- (length map))))
                        (equal (elt map row-a) (elt map row-b)))))      
      (collect reflect))))

(defun test-map (map)
  (>= (+ (length (find-horizontal-reflection map))
         (length (find-vertical-reflection map)))
      1))

(defun swap-char (char)
  (if (char= char #\#) #\. #\#))

(defun get-smudged-map (map)
  (iter outer
    (with orig-horiz = (find-horizontal-reflection map))
    (with orig-vert = (find-vertical-reflection map))
    (for r from 0 below (length map))
    (until (iter
             (for c from 0 below (length (first map)))
             (for cur = (elt (elt map r) c))
             (setf (elt (elt map r) c) (swap-char cur))
             (for horiz =
                   (set-difference (find-horizontal-reflection map) orig-horiz))
             (for vert =
                   (set-difference (find-vertical-reflection map) orig-vert))
             (in outer (finding (list horiz vert) such-that (= 1 (+ (length horiz) (length vert)))))
             (setf (elt (elt map r) c) cur)))))

(defun day13 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for map in parsed)
      (for smudged = (get-smudged-map map))
      (sum  (+ (reduce #'+ (second smudged)) (* 100 (reduce #'+ (first smudged))))))))
