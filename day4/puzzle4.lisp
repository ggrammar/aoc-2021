(require "asdf")

; the first line of the input file is a long list of strings
(defvar bingo-inputs)

; the remaining lines are 5x5 bingo tables that are \n delimited
; the bingo values are space-delimited
(defvar bingo-tables)

; TODO: This creates an array of 100 references to the same 5x5 array.
; I think I can use :element-type to initialize it correctly. In the
; meantime. 
(set 'bingo-tables 
     (make-array 100
		 :initial-element (make-array '(5 5))))

(defun read-bingo-line-into-bingo-table (bingo-table bingo-table-row-index line)
  (let ((bingo-row-iterator 0))
    (loop for bingo-number in (uiop:split-string line :separator " ")
	  do
	   ; TODO: in our input, single-digit numbers are preceded by 
	   ; two spaces, rather than one. There's the delimiting space,
	   ; and the space where a number in the tens column would be.
	   ; I'm sure there's a better way to handle this but, for now,
	   ; we will just skip when bingo-number is an empty string.
	  (if (equal bingo-number "")() ;skip mis-parsed space
	    (progn
	      (setf (aref bingo-table bingo-table-row-index bingo-row-iterator) bingo-number)
	      (incf bingo-row-iterator))))))


(with-open-file (stream "puzzle4.input")
  (let ((bingo-table-index -1)
	(bingo-table-row-index 0))
  (loop for line = (read-line stream nil)
	while line
        do 

	      ; if this is a newline:
	(cond ((eql (length line) 0)
	       ; start work on the next bingo table
	       (incf bingo-table-index)
	       ; start working on index 0 of the new bingo table
	       (setf bingo-table-row-index 0)
	       ; create a *new* table (not a reference to the same one)
	       (setf (aref bingo-tables bingo-table-index) (make-array '(5 5))))

	      ; if this is the input line, store it in bingo-inputs
	      ((eql (aref line 2) #\,)
	       (set 'bingo-inputs 
		    (uiop:split-string line :separator ",")))

	      ; if this line describes some bingo entries
	      ((eql (aref line 2) #\ )
	       (progn
		 (read-bingo-line-into-bingo-table
		   (aref bingo-tables bingo-table-index) 
		   bingo-table-row-index
		   line)
		 (incf bingo-table-row-index)))))))


(defun bingo-table-row-is-solved (bingo-table)
  ; if all 5 entries in a row have an "x", the bingo table is solved
  ; don't need to know "in a row" just need to know that count for the row is 5
  ; look at each row in the table
  (let ((table-has-a-solved-row ()))
    (loop for row from 0 upto 4
	  do
	  ; look at each column in the row
	  (let ((count-of-x-in-row 0))
	    (loop for i from 0 upto 4
		  do (if (equal (aref bingo-table row i) "x")(incf count-of-x-in-row)()))
	    (if (eql count-of-x-in-row 5)(setf table-has-a-solved-row t))))
    (eval table-has-a-solved-row)))

(defun bingo-table-column-is-solved (bingo-table)
  (let ((table-has-a-solved-column ()))
    ; look at each column in the table
    (loop for column from 0 upto 4
	  do
	  (let ((count-of-x-in-column 0))
	    ; look at each row
	    (loop for row from 0 upto 4
		  do (if (equal (aref bingo-table row column) "x")(incf count-of-x-in-column)()))
	    (if (eql count-of-x-in-column 5)(setf table-has-a-solved-column t))))
    (eval table-has-a-solved-column)))

(defun bingo-table-is-solved (bingo-table)
  (or 
    (bingo-table-row-is-solved bingo-table)
    (bingo-table-column-is-solved bingo-table)))


(defun cross-out-bingo-table-input (bingo-table input)
  (loop for row from 0 upto 4
	do (loop for column from 0 upto 4
		 do (if (equal (aref bingo-table row column) input)
		      (setf (aref bingo-table row column) "x")
		      ()))))


(defun score-bingo-table (bingo-table)
  (let ((sum 0))
    (loop for row from 0 upto 4
	  do (loop for column from 0 upto 4
		do 
		(if (equal (aref bingo-table row column) "x")
		     () ; no points for "x"
		     (incf sum (parse-integer (aref bingo-table row column))))))
    (eval sum)))
  

(defvar first-winner)
(set 'first-winner ())
(loop for bingo-input in bingo-inputs
      until (not (equal first-winner ()))
      do
      ; cross out this input on each bingo table
      (loop for bingo-table-index from 0 upto (- (length bingo-tables) 1)
	    do
	    (let ((bingo-table (aref bingo-tables bingo-table-index)))
	      (cross-out-bingo-table-input bingo-table bingo-input)))

      ; check to see if there are any winners for part 1
      (loop for bingo-table-index from 0 upto (- (length bingo-tables) 1)
	    do
	    (let ((bingo-table (aref bingo-tables bingo-table-index)))
	      (if (bingo-table-is-solved bingo-table)
		(set 'first-winner bingo-table)
		()))))

(print first-winner)


(defvar last-winner)
(set 'last-winner (make-array '(5 5) :initial-element 5))
; find the last winner
(let ((tmp-bingo-tables bingo-tables))
  (loop for bingo-input in bingo-inputs
	until (bingo-table-is-solved last-winner)
	do
        (print (length tmp-bingo-tables))
	(print bingo-input)

	; cross out this input on each bingo table
	(loop for bingo-table-index from 0 upto (- (length tmp-bingo-tables) 1)
	      do
	      (let ((bingo-table (aref tmp-bingo-tables bingo-table-index)))
		(cross-out-bingo-table-input bingo-table bingo-input)))

	(setf tmp-bingo-tables (remove-if (lambda (x) (bingo-table-is-solved x)) tmp-bingo-tables))

	(if (equal (length tmp-bingo-tables) 1)(set 'last-winner (aref tmp-bingo-tables 0))())
	(if last-winner (progn
			  (cross-out-bingo-table-input last-winner bingo-input)
			  (if (bingo-table-is-solved last-winner)
			    (print (* (score-bingo-table last-winner) (parse-integer bingo-input)))
			    ())))))

	; need to figure out what the score of the board is *once it wins*













