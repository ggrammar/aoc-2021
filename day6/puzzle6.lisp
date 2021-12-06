(require "asdf")

(defvar initial-fish)
(set 'initial-fish
     (make-array 300 :initial-contents
		 (with-open-file (stream "puzzle6.input")
		   (map 'list 'parse-integer (uiop:split-string (read-line stream nil) :separator ",")))))

(defvar fish)
(set 'fish (make-array 9))

(loop for i from 0 to 8 do
      (setf (aref fish i) (length (remove-if-not (lambda (x) (equal x i)) initial-fish))))

(defun generation (a)
  (declare (array a))
  (let ((spawning-fish (aref a 0)))
    (loop for i from 0 upto 7 do
	  (setf (aref a i)(aref a (+ 1 i))))
    (setf (aref a 8) spawning-fish)
    (incf (aref a 6) spawning-fish)))

(loop for day from 0 to 80 do (generation fish))
(format T "After 80 days, there are ~d fish." (apply '+ (loop for i from 0 to 7 collect (aref fish i))))(terpri)

(loop for day from 81 to 256 do (generation fish))
(format T "After 256 days, there are ~d fish." (apply '+ (loop for i from 0 to 7 collect (aref fish i))))(terpri)

      

