(require "asdf")

(defvar initial-fish)
(set 'initial-fish
     (make-array 300 :initial-contents
		 (with-open-file (stream "puzzle6.input")
		   (map 'list 'parse-integer (uiop:split-string (read-line stream nil) :separator ",")))))

(defvar fish)
(set 'fish (make-array 9))

; Rather than keep track of every single fish (initial-fish), we can just keep
; track of the count of fish at each timer interval. We'll convert initial-fish
; into an array that contains the count at each timer interval (fish), and 
; operate on that. 
(loop for i from 0 to 8 do
      (setf (aref fish i) (length (remove-if-not (lambda (x) (equal x i)) initial-fish))))

(defun generation (a)
  (let ((spawning-fish (aref a 0)))
    ; working upwards, decrement each timer. 
    (loop for i from 0 upto 7 do
	  (setf (aref a i)(aref a (+ 1 i))))
    ; the only fish at timer 8 were just spawned, so setf
    (setf (aref a 8) spawning-fish)
    ; fish at timer 6 may be spawners or decremented from 7, so incf
    (incf (aref a 6) spawning-fish)))

(defun count-all-fish (a)
  (apply '+ (loop for i from 0 to 7 collect (aref a i))))

(loop for day from 0 to 80 do (generation fish))
(format T "After 80 days, there are ~d fish." (count-all-fish fish))(terpri)

(loop for day from 81 to 256 do (generation fish))
(format T "After 256 days, there are ~d fish." (count-all-fish fish))(terpri)

      

