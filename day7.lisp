(in-package :aoc-2023)

(defun parse-card ()
  (either (parse-digit)
          (bind (parse-alphanumeric) (lambda (c) (unit (intern (string (char-upcase c)) :keyword))))))

(defun count-hand (hand)
  (iter
    (with ret = (make-hash-table))
    (for card in hand)
    (incf (gethash card ret 0))
    (finally (return (sort (alexandria:hash-table-alist ret) #'> :key #'cdr)))))

(defun hand-type (hand)
  (let ((counts (count-hand hand)))
    (cond
      ((= 1 (length counts)) :five-of-a-kind)
      ((= 4 (cdr (first counts))) :four-of-a-kind)
      ((and (= 3 (cdr (first counts)))
            (= 2 (cdr (second counts)))) :full-house)
      ((= 3 (cdr (first counts))) :three-of-a-kind)
      ((and (= 2 (cdr (first counts)))
            (= 2 (cdr (second counts)))) :two-pair)
      ((= 2 (cdr (first counts))) :one-pair)
      (t :high-card))))

(defun hand-type-2 (hand)
  (hand-type (get-best-hand hand)))

(defun num-jokers (hand)
  (iter
    (for card in hand)
    (counting (eq :j card))))

(defun compare-cards (a b)
  (let ((order '(:a :k :q :j :t 9 8 7 6 5 4 3 2)))
    (- (position b order) (position a order))))

(defun compare-cards-2 (a b)
  (let ((order '(:a :k :q :t 9 8 7 6 5 4 3 2 :j)))
    (- (position b order) (position a order))))

(defun compare-hands (a b)
  (let* ((order '(:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind
                  :two-pair :one-pair :high-card))
         (type-compare (- (position (hand-type b) order)                          
                          (position (hand-type a) order))))
    (if (= 0 type-compare)
        (let ((card-compare (iter
                              (for card-a in a)
                              (for card-b in b)
                              (for card-compare = (compare-cards card-a card-b))
                              (finding card-compare such-that (not (= 0 card-compare))))))
          (if card-compare card-compare 0))        
        type-compare)))

(defun compare-hands-2 (a b type-a type-b)
  (let* ((order '(:five-of-a-kind :four-of-a-kind :full-house :three-of-a-kind
                  :two-pair :one-pair :high-card))
         (type-compare (- (position type-b order)                          
                          (position type-a order))))
    (if (= 0 type-compare)
        (let ((card-compare (iter
                              (for card-a in a)
                              (for card-b in b)
                              (for card-compare = (compare-cards-2 card-a card-b))
                              (finding card-compare such-that (not (= 0 card-compare))))))
          (if card-compare card-compare 0))        
        type-compare)))

(defun get-all-hands (hand-so-far hand-remaining)
  (if (null hand-remaining)
      (list hand-so-far)
      (if (not (eq :j (car hand-remaining)))
          (get-all-hands (cons (car hand-remaining) hand-so-far)
                         (cdr hand-remaining))
          (iter
            (for card in '(:a :k :q :j :t 9 8 7 6 5 4 3 2))
            (appending (get-all-hands (cons card hand-so-far)
                                      (cdr hand-remaining)))))))

(defun get-best-hand (hand)
  (let ((all-hands (get-all-hands '() hand)))
    (first (sort all-hands
                 (lambda (a b) (>= (compare-hands a b) 0))))))

(defun parse-file ()
  (parse-lines
   (with-monad
     (assign cards (n-of 5 (parse-card)))
     (parse-space)
     (assign bid (parse-number))
     (unit (list cards bid)))))

(defun day7 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (part2 (iter
                  (for (hand bid) in parsed)
                  (collect (list hand (hand-type-2 hand) bid)))))
    (iter
      (for (hand type bid) in
           (sort part2
                 (lambda (a b) (<= (compare-hands-2 (first a) (first b) (second a) (second b)) 0))))
      (for rank from 1)
      (summing (* bid rank)))
    ))
