(defvar puzzle-input)
(set 'puzzle-input
     (with-open-file (stream "puzzle3.input")
       (loop for line = (read-line stream nil)
	     while line
	     collect line)))

(defun get-popular-at-n (l n default)
  (declare (list l))
  (declare (number n))

  (let ((number-of-zeroes (length (remove-if (lambda (x) (equal (aref x n) #\1)) l)))
	(half-of-list (/ (length l) 2)))
    (cond ((> number-of-zeroes half-of-list)(eval #\0))
	  ((< number-of-zeroes half-of-list)(eval #\1))
	  ((= number-of-zeroes half-of-list)(eval default)))))

; part 1 - print these out and manually reverse
; (loop for i from 0 upto 11 do (print (get-popular-at-n puzzle-input i #\1)))



; part 2
(defun calculate-rating (l type-of-rating)
  (declare (list l))
  (declare (string type-of-rating))

  (let ((filter-list l))
    (loop for n from 0 upto (- (length filter-list) 1)
	  until (= 1 (length filter-list)) do
	  (if (equal "o2" type-of-rating)
	    (setf filter-list 
		  (remove-if-not (lambda(x) (equal (get-popular-at-n filter-list n #\1) (aref x n))) filter-list))
	    (setf filter-list 
		  (remove-if     (lambda(x) (equal (get-popular-at-n filter-list n #\1) (aref x n))) filter-list))))
    (car filter-list)))

; print these out and manually convert and multiply them
(print (calculate-rating puzzle-input "o2"))
(print (calculate-rating puzzle-input "co2"))

; https://stackoverflow.com/questions/34219181/binary-to-decimal-in-lisp-using-a-non-nested-list

