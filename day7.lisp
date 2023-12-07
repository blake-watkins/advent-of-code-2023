(in-package :aoc-2023)

(defun parse-card ()
  (either (parse-digit)
          (bind (parse-alphanumeric)
                (lambda (c) (unit (intern (string (char-upcase c)) :keyword))))))

(defun parse-file ()
  (parse-lines
   (with-monad
     (assign cards (n-of 5 (parse-card)))
     (parse-space)
     (assign bid (parse-number))
     (unit (list cards bid)))))

(defparameter *part-1-order* '(:a :k :q :j :t 9 8 7 6 5 4 3 2))
(defparameter *part-2-order* '(:a :k :q :t 9 8 7 6 5 4 3 2 :j))
(defparameter *hand-types* '(((5) :five-of-a-kind)
                             ((4 1) :four-of-a-kind)
                             ((3 2) :full-house)
                             ((3 1 1) :three-of-a-kind)
                             ((2 2 1) :two-pair)
                             ((2 1 1 1) :pair)
                             ((1 1 1 1 1) :high-card)))

(defun count-cards (hand)
  (iter
    (with ret = (make-hash-table))
    (for card in hand)
    (incf (gethash card ret 0))
    (finally (return (sort (alexandria:hash-table-alist ret) #'> :key #'cdr)))))

(defun hand-type (hand part)
  (let* ((type-and-count (count-cards hand))
         (count
           (if (= part 1)
               (mapcar #'cdr type-and-count)
               (let ((jokers (find :j type-and-count :key #'car))
                     (without-jokers (remove :j type-and-count :key #'car)))
                 (cond
                   ((and jokers (= 5 (cdr jokers))) '(5))
                   (jokers (cons (+ (cdr (first without-jokers)) (cdr jokers))
                                 (mapcar #'cdr (rest without-jokers))))
                   (t (mapcar #'cdr without-jokers)))))))
    (iter
      (for (pattern type) in *hand-types*)
      (finding type such-that (equal pattern count)))))

(defun compare-hands (a b part)
  (let ((hand-type-compare
          (- (position (hand-type b part) *hand-types* :key #'second)
             (position (hand-type a part) *hand-types* :key #'second))))
    (if (= 0 hand-type-compare)
        (let ((order (if (= part 1) *part-1-order* *part-2-order*)))
          (or (iter
                (for card-a in a)
                (for card-b in b)
                (for card-compare = (- (position card-b order)
                                       (position card-a order)))
                (finding card-compare such-that (not (= 0 card-compare))))
              0))
        hand-type-compare)))

(defun day7 (input &key (part 1))
  (let* ((parsed (run-parser (parse-file) input)))
    (iter
      (for (hand bid) in
           (sort parsed
                 (lambda (a b) (<= (compare-hands a b part) 0))
                 :key #'first))
      (for rank from 1)
      (summing (* bid rank)))))
