(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines
   (with-monad
     (assign springs (one-or-more
                      (either (then (parse-character #\.)  (unit :works))
                              (then (parse-character #\#)  (unit :damaged))
                              (then (parse-character #\?) (unit :unknown)))))
     (parse-whitespace)
     (assign condition (parse-number-list))
     (unit (list springs condition)))))

(defun expand-springs (springs)
  (rest (iter (repeat 5) (appending (cons :unknown springs)))))

(defun expand-condition (condition)
  (iter (repeat 5) (appending condition)))

(defparameter *store* (make-hash-table :test 'equal))

(defun num-valid-rec (damaged-so-far springs condition)
  (if (null springs)
      (if (or (and (zerop damaged-so-far) (null condition))
              (equal (list damaged-so-far) condition))
          1 0)
      (case (first springs)
        (:damaged
         (num-valid (1+ damaged-so-far) (rest springs) condition))
        (:works
         (if (plusp damaged-so-far)
             (if (or (null condition) (not (= damaged-so-far (first condition))))
                 0
                 (num-valid 0 (rest springs) (rest condition)))
             (num-valid 0 (rest springs) condition)))
        (:unknown                  
         (+ (num-valid damaged-so-far (cons :damaged (rest springs)) condition)
            (num-valid damaged-so-far (cons :works (rest springs)) condition))))))

(defun num-valid (damaged-so-far springs condition)
  (let ((cached (gethash (list damaged-so-far springs condition) *store*)))
    (if cached
        cached
        (let ((ret (num-valid-rec damaged-so-far springs condition)))
          (setf (gethash (list damaged-so-far springs condition) *store*) ret)
          ret))))

(defun day12 (input &key (part 1))
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for (springs condition) in parsed)
      (when (= part 2)
        (setf springs (expand-springs springs))
        (setf condition (expand-condition condition)))
      (setf *store* (make-hash-table :test 'equal))
      (sum (num-valid 0 springs condition)))))
