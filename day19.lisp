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
    (unit (cons name rules))))

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

(defun matches-rule (rule ratings)
  "If RATINGS match RULE return the destination workflow, otherwise NIL."
  (if (symbolp rule)
      rule
      (destructuring-bind (part-name op val dest) rule
        (let ((part-value (second (assoc part-name ratings))))
          (when (funcall (case op (#\< #'<) (#\> #'>)) part-value val)
            dest)))))

(defun destination (workflow ratings workflows)
  "Return whether RATINGS are accepted (:A) or rejected (:R) when starting from WORKFLOW. "
  (if (member workflow '(:a :r))
      workflow
      (let* ((rules (gethash workflow workflows))
             (next-destination (iter
                                 (for rule in rules)
                                 (thereis (matches-rule rule ratings)))))
        (destination next-destination ratings workflows))))

(defun add-part-ratings (parts)
  "Add each rating for each part in PARTS. "
  (iter 
    (for part in parts)
    (summing (reduce #'+ part :key #'second))))

(defun count-ratings-combinations (ratings-ranges)
  "Count combinations by multiplying range size of each rating. "
  (reduce #'* (mapcar #'second ratings-ranges) :key #'interval-size))

(defun get-range (rule accept-reject)
  "Return (part-name range) list corresponding to acceptance/rejection of RULE."
  (destructuring-bind (part-name op val dest) rule
    (declare (ignore dest))
    (list part-name
          (case op
            (#\< (if (eq accept-reject :accept) `(1 ,(1- val)) `(,val 4000)))
            (#\> (if (eq accept-reject :accept) `(,(1+ val) 4000) `(1 ,val)))))))

(defun limit-ratings (limit ratings)
  "Intersect the rating in RATINGS by the (part-name range) LIMIT. "
  (destructuring-bind (limit-name limit-range) limit
    (iter
      (for rating in ratings)
      (for (part-name part-range) = rating)
      (collect
          (if (eq part-name limit-name)
              (list part-name (interval-intersect part-range limit-range))
              rating)))))

(defun count-accepted (workflow accepted workflows)
  "Get all combinations of accepted ratings starting from WORKFLOW with currently ACCEPTED ratings. "
  (case workflow
    (:a (count-ratings-combinations accepted))
    (:r 0)
    (otherwise
     (iter
        (for rule in (gethash workflow workflows))
        (if (symbolp rule)
            (summing (count-accepted rule accepted workflows))
            (let ((accept (limit-ratings (get-range rule :accept) accepted))
                  (reject (limit-ratings (get-range rule :reject) accepted)))
              (summing (count-accepted (fourth rule) accept workflows))
              (setf accepted reject)))))))

(defun day19 (input &key (part 1))
  (destructuring-bind (workflows ratings) (run-parser (parse-file) input)
    (let ((workflow-table (hash-table-from-alist workflows)))
      (if (= part 1)
          (add-part-ratings
           (iter
             (for rating in ratings)
             (when (eq :a (destination :in rating workflow-table))
               (collect rating))))
          (count-accepted :in
                          (mapcar (lambda (n) `(,n (1 4000))) '(:x :m :a :s))
                          workflow-table)))))

