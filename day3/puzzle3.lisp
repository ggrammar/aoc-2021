
(defvar puzzle-input)
(set 'puzzle-input
     (with-open-file (stream "puzzle3.input")
       (loop for line = (read-line stream nil)
	     while line
	     collect line)))

(defun get-popular-at-n (l n default)
  (declare (list l))
  (declare (number n))
  (declare (character default))
  (terpri)

  (let ((number-of-zeroes (length (remove-if (lambda (x) (equal (aref x n) #\1)) l)))
	(half-of-list (/ (length l) 2)))
    (format T "number-of-zeroes ~d , half-of-list ~d" number-of-zeroes half-of-list)(terpri)
    (cond ((> number-of-zeroes half-of-list)(eval #\0))
	  ((< number-of-zeroes half-of-list)(eval #\1))
	  ((= number-of-zeroes half-of-list)(print "equally common")(eval default)))))

; part 1
; (loop for i from 0 upto 11 do
      ; (print (get-popular-at-n puzzle-input i)))



; part 2

; oxygen generator rating
; keep numbers with the most common value, default to 1 if equally common
(let ((filter-list puzzle-input))
  (loop for n from 0 upto 11 do
	(let ((popular-at-n (get-popular-at-n filter-list n #\1)))
	  (format T "popular at ~d is ~d" n popular-at-n)(terpri)
	  (format T "before filtering, ~d entries, " (length filter-list))
	  (setf filter-list (remove-if-not (lambda(x) (equal popular-at-n (aref x n))) filter-list))
	  (format T "after filtering, ~d entries" (length filter-list))(terpri)
	  (if (equal 1 (length filter-list))(print filter-list)()))))

(terpri)(terpri)

; co2 scrubber rating
; keep numbers with the least common value, default to 0 if equally common
(let ((filter-list puzzle-input))
  (loop for n from 0 upto 11 do
	(let ((popular-at-n (get-popular-at-n filter-list n #\1)))
	  (format T "popular at ~d is ~d" n popular-at-n)(terpri)
	  (format T "before filtering, ~d entries, " (length filter-list))
	  (setf filter-list (remove-if (lambda(x) (equal popular-at-n (aref x n))) filter-list))
	  (format T "after filtering, ~d entries" (length filter-list))(terpri)
	  (if (equal 1 (length filter-list))(print filter-list)()))))
