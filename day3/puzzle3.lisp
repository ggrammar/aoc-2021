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
	      (set 'total-input-lines (+ total-input-lines 1)))())))))

(format T "gamma rate (binary)   :: ")
(loop for i from 0 upto 11
      collect
      (if (> (aref result-array i) 500)(write 1)(write 0)))(terpri)

(format T "epsilon rate (binary) :: ")
(loop for i from 0 upto 11
      collect
      (if (> (aref result-array i) 500)(write 0)(write 1)))(terpri)

; I manually converted these to decimal to multiply them.
(write (* 1836 2259))

