(in-package :aoc-2023)

(defun parse-file ()
  (parse-list
   (parse-characters (lambda (c) (not (or (char= c #\,) (whitespace-char-p c)))))))

(defun parse-file-2 ()
  (parse-list
   (with-monad
    (assign label (parse-characters #'alphanumericp))
    (assign operation (either (then (parse-character #\-) (unit (list :remove)))
                              (with-monad
                                (parse-character #\=)
                                (assign focal-length (parse-number))
                                (unit (list :insert focal-length)))))
    (unit (cons label operation)))))

(defun hash (str)
  (iter
    (with current-value = 0)
    (for char in-string str)
    (setf current-value (mod (* (+ current-value (char-code char)) 17) 256))
    (finally (return current-value))))

(defun focusing-power (boxes)
  (iter outer
    (for (box-number lenses) in-hashtable boxes)
    (iter
      (for (label focal-length) in (reverse lenses))
      (for slot from 1)
      (in outer (summing (* (1+ box-number) slot focal-length))))))

(defun day15 (input &key (part 1))
  (if (= part 1)
      (iter
        (for step in (run-parser (parse-file) input))
        (summing (hash step)))
      (iter
        (with boxes = (make-hash-table))
        (for (label operation value) in (run-parser (parse-file-2) input))
        (for label-hash = (hash label))
        (for lenses = (gethash label-hash boxes))
        (case operation
          (:remove
           (setf (gethash label-hash boxes)
                 (remove label lenses :test #'string= :key #'first)))
          (:insert
           (let ((position (position label lenses :test #'string= :key #'first)))
             (if position
                 (setf (second (elt lenses position)) value)
                 (push (list label value) (gethash label-hash boxes))))))
        (finally (return (focusing-power boxes))))))
