; needs to be run with `--control-stack-size 5` to increase the control stack size,
; otherwise it bails around day 75. 

(require "asdf")

(defvar fish)

(set 'fish 
     (with-open-file (stream "puzzle6.input")
       (map 'list 'parse-integer (uiop:split-string (read-line stream nil) :separator ","))))

(defvar num-days)
(set 'num-days 80)

(defun iterate-fish (f) (- f 1))
(defun reset-timer (f) (if (= f -1)(eval 6)(eval f)))

(format T "Initial state, there are ~d fish." (length fish))(terpri)
(let ((fish-to-populate 0))
  (loop for day from 1 to num-days do
	; decrement every fish
	(set 'fish (map 'list (lambda (x) (iterate-fish x)) fish))

	; count how many we need to add, and add them
	(setf fish-to-populate (apply '+ (loop for f in fish collect (if (= f -1)(eval 1)(eval 0)))))
	(set 'fish (append fish (make-list fish-to-populate :initial-element 8)))

	; reset timers
	(set 'fish (map 'list (lambda (x) (reset-timer x)) fish))
	(format T "After ~d days, there are ~d fish." day (length fish))(terpri)))



