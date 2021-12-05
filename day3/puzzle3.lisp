(defvar total-input-lines)
(set 'total-input-lines 0)

; get the length of the first input value, to figure out how many variables we need
; (length "asdf")
; for char in line
;   ; 
;   (incf (nth result index) (parse-integer char))
;   increment index
; Create a list with 12 zeroes, where we will track the count of ones. 
(defvar result-array)
(set 'result-array (make-array '(12)))

; this iterates through each byte of the file, rather than each line like I've
; been doing. 
(with-open-file (stream "puzzle3.input" :element-type '(unsigned-byte 8))
  (let ((char-iterator 0))
    (loop for b = (read-byte stream nil)
      while b
      collect
	(progn
	  ; if the byte is ascii 1, increment the appropriate index in the array. 
	  (if (eq b 49)(incf (aref result-array char-iterator))())
	  ; we don't need to do anything for ascii 0 - we can figure out if there
	  ; are more zeroes than ones, based on the number of ones and total lines.

	  ; next byte, next index in the array. 
	  (incf char-iterator)
	  ; if we've reached the end of the string (ascii \n), reset. 
	  (if (eq b 10)
	    (progn
	      (setf char-iterator 0)
	      (set 'total-input-lines (+ total-input-lines 1)))
	    ())))))

(write result-array)(terpri)

; TODO: Figure out how to just collect 1s and 0s from this loop, so
; we can handle this without the calls to `write`. It will be useful
; to have these values available for part 2. 
(format T "gamma rate (binary)   :: ")
(loop for i from 0 upto 11
      collect
      (if (> (aref result-array i) 500)(write 1)(write 0)))(terpri)

(format T "epsilon rate (binary) :: ")
(loop for i from 0 upto 11
      collect
      (if (> (aref result-array i) 500)(write 0)(write 1)))(terpri)

; I manually converted these to decimal to multiply them.
(format T "power consumption (decimal) :: ~d" (* 1836 2259))(terpri)


; oxygen generator rating
;   what is the most common value for this bit position? (found in gamma rate)
;     keep only numbers with that bit in that position. 

; co2 scrubber rating
;   what is the least common value for this bit position? (found in epsilon rate)
;     keep only numbers with that bit in that position

(defvar filter-list)
(set 'filter-list
     (with-open-file (stream "puzzle3.input")
       (loop for line = (read-line stream nil)
	     while line
	     collect line)))

; this should be the only place a char actually appears
(defun line-at-index-is-one (line index)
  (declare (vector line))
  (declare (number index))
  (equal (aref line index) #\1))
       
(defun get-value-at-index (line index)
  (declare (vector line))
  (declare (number index))
  (if (line-at-index-is-one line index)
    (eval 1)
    (eval 0)))

(defun get-popular-at-index (some-list index)
  (declare (list some-list))
  (declare (number index))
  ; sum the numbers at that index
  (let (( some-sum (apply '+ (map 'list (lambda (x) (get-value-at-index x index)) some-list)))
	( half-of-list (/ (length some-list) 2)))
    (if (> some-sum half-of-list)
      (eval 1)
      (eval 0))))

; TODO: Handle the situation where 0 and 1 are equally common. 
(let ((tmp-filter-list filter-list))
  (loop for i from 0 to 11 do
	(print (length tmp-filter-list))
	(setf tmp-filter-list 
	      (remove-if 
		(lambda (line) 
		  (equal 
		    (if (line-at-index-is-one line i)(eval 1)(eval 0))
		    (get-popular-at-index tmp-filter-list i)))
		tmp-filter-list))
	(print (length tmp-filter-list))))


; (print (apply '+ (map 'list (lambda (x) (get-value-at-index x 0)) filter-array)))

























