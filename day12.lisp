(in-package :aoc-2023)

(defun parse-file ()
  (parse-lines
   (with-monad
     (assign springs (one-or-more
                      (either (then (parse-character #\.)  (unit :operational))
                              (then (parse-character #\#)  (unit :damaged))
                              (then (parse-character #\?) (unit :unknown)))))
     (parse-whitespace)
     (assign condition (parse-number-list))
     (unit (list springs condition)))))

(defun is-valid? (springs condition)
  (equal (mapcar #'length (split-sequence:split-sequence :operational springs :remove-empty-subseqs t))
         condition))

(defun find-valid-combinations (springs-acc springs condition)
  (if (null springs)
      (if (is-valid? (reverse springs-acc) condition) 1 0)
      (let ((spring (first springs)))
        (if (not (eq :unknown spring))
            (find-valid-combinations (cons spring springs-acc)
                                     (rest springs)
                                     condition)
            (+ (find-valid-combinations (cons :operational springs-acc)
                                        (rest springs)
                                        condition)
               (find-valid-combinations (cons :damaged springs-acc)
                                        (rest springs)
                                        condition))))))

(defun expand-springs (springs)
  (rest (iter
          (repeat 5)
          (appending '(:unknown))
          (appending springs))))

(defun expand-condition (condition)
  (iter
    (repeat 5)
    (appending condition)))
(defparameter *store* (make-hash-table :test 'equal))

(defun find-valid-2 (damaged-so-far springs condition)
  (let ((cached (gethash (list damaged-so-far springs condition) *store*)))
    (if cached
        cached
        (let ((ret (if (null springs)
                       (if (or (and (zerop damaged-so-far) (null condition))
                               (equal (list damaged-so-far) condition))
                           1 0)
                       (case (first springs)
                         (:damaged
                          (find-valid-2 (1+ damaged-so-far) (rest springs) condition))
                         (:operational
                          (if (plusp damaged-so-far)
                              (if (or (null condition)
                                      (not (= damaged-so-far (first condition))))
                                  0
                                  (find-valid-2 0 (rest springs) (rest condition)))
                              (find-valid-2 0 (rest springs) condition)))
                         (:unknown
                          (+ (find-valid-2 damaged-so-far (cons :damaged (rest springs)) condition)
                             (find-valid-2 damaged-so-far (cons :operational (rest springs)) condition)))))))
          (setf (gethash (list damaged-so-far springs condition) *store*) ret)
          ret))))
