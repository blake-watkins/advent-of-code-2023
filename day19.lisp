(in-package :aoc-2023)

(defun parse-name ()
  (parse-keyword #'alpha-char-p))

(defun parse-rule ()
  (either (with-monad
            (assign part-name (parse-name))
            (assign op (parse-character "><"))
            (assign val (parse-number))
            (parse-character ":")
            (assign dest (parse-name))
            (unit (list part-name op val dest)))
          (parse-name)))

(defun parse-workflow ()
  (with-monad
    (assign name (parse-name))
    (assign rules (parse-bracketed (parse-list (parse-rule)) "{}"))
    (unit (list name rules))))

(defun parse-rating ()
  (with-monad
    (assign name (parse-name))
    (parse-character #\=)
    (assign val (parse-number))
    (unit (list name val))))

(defun parse-ratings ()
  (parse-bracketed (parse-list (parse-rating)) "{}"))

(defun parse-file ()
  (with-monad
    (assign workflows (parse-lines (parse-workflow)))
    (parse-newline)
    (parse-newline)
    (assign ratings (parse-lines (parse-ratings)))
    (unit (list workflows ratings))))

(defun part-value (name ratings)
  (second (assoc name ratings)))

(defun matches-rule (rule ratings)
  (if (symbolp rule)
      rule
      (destructuring-bind (part-name op val dest) rule
        (when (funcall (case op (#\< #'<) (#\> #'>))
                       (part-value part-name ratings)
                       val)
          dest))))

(defun destination (workflow ratings workflows)
  (if (member workflow '(:a :r))
      workflow
      (let* ((rules (gethash workflow workflows))
             (next-destination (iter
                                 (for rule in rules)
                                 (thereis (matches-rule rule ratings)))))
        (destination next-destination ratings workflows))))

(defun sum-ratings (accepted)
  (reduce #'+ (mapcar (lambda (ratings) (reduce #'+ (mapcar #'second ratings))) accepted)))

(defun count-accepted-ratings (accepted)
  (reduce #'* (mapcar (lambda (x) (interval-size (second x))) accepted)))

(defun intersect-ratings (accepted-a accepted-b)
  (map 'list (lambda (a b)
               (list (first a) (interval-intersect (second a) (second b))))
       accepted-a
       accepted-b))

(defun accept-range (rule)
  (destructuring-bind (part-name op val dest) rule
    (declare (ignore dest))
    (case op
      (#\< (list part-name (list 0 (1- val))))
      (#\> (list part-name (list val 4000))))))

(defun reject-range (rule)
  (destructuring-bind (part-name op val dest) rule
    (declare (ignore dest))
    (case op
      (#\< (list part-name (list val 4000)))
      (#\> (list part-name (list 0 (1- val)))))))

(defun limit-ratings (range ratings)
  (destructuring-bind (range-name range-range) range
    (mapcar (lambda (rating)
              (destructuring-bind (part-name rating-range) rating
                (if (eq part-name range-name)
                    (list part-name (interval-intersect range-range
                                                        rating-range))
                    rating)))
            ratings)))

(defun count-accepted (workflow prev-workflows accepted workflows)
  (if (member workflow prev-workflows)
      0
      (iter
        (for rule in (gethash workflow workflows))
        (summing
         (if (symbolp rule)
             (ecase workflow
               (:a (count-accepted-ratings accepted))
               (:r 0))
             (let* ((accept-range (accept-range rule))
                    (reject-range (reject-range rule))
                    (num-accepted (count-accepted (fourth rule)
                                                  (cons workflow prev-workflows)
                                                  (limit-ratings accept-range
                                                                 accepted)
                                                  workflows)))
               (setf accepted (limit-ratings reject-range accepted))
               num-accepted))))))
(defun day19 (input)
  (destructuring-bind (workflows ratings) (run-parser (parse-file) input)
    (let ((workflow-table (iter
                            (with ret = (make-hash-table))
                            (for (name rules) in workflows)
                            (setf (gethash name ret) rules)
                            (finally (return ret)))))
      (sum-ratings
       (iter
         (for rating in ratings)
         (when (eq :a (destination :in rating workflow-table))
           (collect rating))))
      (count-accepted :crn '((:x (0 4000)) (:m (0 4000)) (:a (0 4000)) (:s (0 4000))) workflow-table))))

