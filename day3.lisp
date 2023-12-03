(in-package :aoc-2023)

(defun parse-indexed-number ()
  (with-monad
    (assign start-rc (parse-rc-get-rc))
    (assign num (lift-parser-rc (with-monad
				  (assign digits (one-or-more (parse-digit)))
				  (unit (digits-to-int digits :base 10)))))
    (assign end-rc (parse-rc-get-rc))
    (unit (list :number (list start-rc end-rc) num))))

(defun parse-line ()
  (with-monad
    (assign ret
	    (one-or-more
	     (either (parse-indexed-number)
		     (then (parse-rc-character ".") (unit '()))
		     (with-monad
		       (assign sym (parse-rc-character (complement #'whitespace-char-p)))
		       (unit (cons :symbol sym))))))
    (parse-rc-newline)
    (unit ret)))

(defun number-near-symbol? (number symbol)
  (destructuring-bind (((sr sc) (er ec)) val) number
    (declare (ignore er val))
    (destructuring-bind ((r c) sym) symbol
      (declare (ignore sym))
      (and (<= (1- sc) c ec) (<= (1- sr) r (1+ sr))))))

(defun numbers-near-symbol (numbers symbol)
  (iter
    (for number in numbers)
    (when (number-near-symbol? number symbol)
      (collect number))))

(defun gear-ratio (numbers symbol)
  (when (char= #\* (second symbol))
    (let ((nearby-numbers (numbers-near-symbol numbers symbol)))
      (when (= 2 (length nearby-numbers))
	(reduce #'* nearby-numbers :key #'second)))))

(defun day3 (input &key (part 1))
  (let ((parsed (run-rc-parser (one-or-more (parse-line)) input)))
    (destructuring-bind (numbers symbols)
	(iter outer
	  (for line in parsed)
	  (iter
	    (for (type . item) in line)
	    (case type
	      (:number (in outer (collect item into numbers)))
	      (:symbol (in outer (collect item into symbols)))))
	  (finally (return-from outer (list numbers symbols))))
      (if (= part 1)
	  (iter outer
	    (for number in numbers)
	    (iter
	      (for symbol in symbols)
	      (when (number-near-symbol? number symbol)
		(in outer (sum (second number))))))
	  (iter
	    (for symbol in symbols)
	    (for gear-ratio = (gear-ratio numbers symbol))
	    (when gear-ratio (sum gear-ratio)))))))
