(require "asdf")

(defvar puzzle-input)
(set 'puzzle-input
     (with-open-file (stream "puzzle8.input")
       (loop for line = (read-line stream nil) 
	     while line collect (eval line))))


; digits we can recognize just based on number of segments. 
(defun parse-simple-digit (digit)
  (declare (string digit))
  (cond ((equal (length digit) 2) (eval 1))
	((equal (length digit) 3) (eval 7))
	((equal (length digit) 4) (eval 4))
	((equal (length digit) 7) (eval 8))))


; this function returns a list of digits in the output
(defun parse-output-value (output)
  (declare (string output))
  ; parse-simple-digit returns the string if we don't recognize the digit, so we'll
  ; remove nil before returning the array of digits we do recognize. 
  (remove nil 
	  (map 'list 
	       (lambda (x) (parse-simple-digit x)) 
	       (uiop:split-string output :separator " "))))

(defun sort-signal-pattern (signal-pattern)
  (loop for sig in (uiop:split-string signal-pattern :separator " ") collect
	(sort (copy-seq sig) #'char-lessp)))

; solution for part 1
(format T "The digits 1, 4, 7, and 8 appear ~d times."
	; count of digits that we're able to recognize across all puzzle inputs. 
	(apply '+ 
	       (loop for pattern in puzzle-input collect
		     ; for simple digits, we can ignore the signal patterns, and just
		     ; look at the output value. 
		     ; collecting length, so the number of outputs that we recognize. 
		     (length (parse-output-value
			       (nth 1 (uiop:split-string pattern :separator "|")))))))
(terpri)

(defun num-signals-present (sig-1 sig-2)
  (loop for i from 0 upto (- (length sig-1) 1) count
	(find (char sig-1 i) sig-2)))

(defun signal-to-digit (sig signal-pattern)
  (declare (string sig))
  (declare (list signal-pattern))
  (let ((sig-one (car (remove-if-not (lambda (x) (equal 2 (length x))) signal-pattern)))
	(sig-four (car (remove-if-not (lambda (x) (equal 4 (length x))) signal-pattern))))

    ; we can evaluate these signals without the full signal pattern
    (cond ((equal (length sig) 2) (eval 1))
          ((equal (length sig) 3) (eval 7))
	  ((equal (length sig) 4) (eval 4))
	  ((equal (length sig) 7) (eval 8))

	  ; signal length five might be 2, 3, or 5
	  ((equal (length sig) 5)
	         ; only 3 has both signals from 1
	   (cond ((equal 2 (num-signals-present sig sig-one))(eval 3))
		 ; 2 shares two signals with 4
		 ((equal 2 (num-signals-present sig sig-four))(eval 2))
		 ; 5 shares three signals with 4
		 ((equal 3 (num-signals-present sig sig-four))(eval 5))))

	  ; signal length 6 might be 0, 6, or 9
	  ((equal (length sig) 6)
	   ; 9 shares four signals with 4
	   (cond ((equal 4 (num-signals-present sig sig-four))(eval 4))
		 ; 6 shares one with 1
		 ((equal 1 (num-signals-present sig sig-one))(eval 6))
		 ((equal t t)(eval 0)))))))

; need something that accepts an output list and returns a number
(defun parse-output (output-signals signal-pattern)
  (concatenate 'list (loop for o in output-signals collect
	(signal-to-digit o signal-pattern))))

(print (apply '+ (loop for input in puzzle-input collect
      (let ((input-signals  (nth 0 (uiop:split-string input :separator "|")))
	    (output-signals (nth 1 (uiop:split-string input :separator "|")))) 

	(let ((parsed-output-signals (parse-output
				     (cdr (uiop:split-string output-signals :separator " "))
				     (uiop:split-string input-signals :separator " "))))
	  ; turn the list of 4 integers into one integer - what a mess!      
	  (+ (* 1000 (nth 0 parsed-output-signals))
	     (* 100  (nth 1 parsed-output-signals))
	     (* 10   (nth 2 parsed-output-signals))
	     (* 1    (nth 3 parsed-output-signals))))))))










