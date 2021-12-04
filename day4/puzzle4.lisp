(require "asdf")

; the first line of the input file is a long list of strings
(defvar bingo-inputs)

; the remaining lines are 5x5 bingo tables that are \n delimited
; the bingo values are space-delimited
(defvar bingo-tables)

(set 'bingo-tables 
     (make-array 100
		 :initial-element (make-array '(5 5))))

; a bingo table is a 5x5 

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
	       (setf bingo-table-row-index 0))

	      ; if this is the input line, store it in bingo-inputs
	      ((eql (aref line 2) #\,)
	       (set 'bingo-inputs line))

	      ; if this line describes some bingo entries
	      ((eql (aref line 2) #\ )
	       (progn
		 ; bingo-table is the table we're currently working on
		 (let ((bingo-table (aref bingo-tables bingo-table-index))
		       ; bingo-row-iterator is the table row (0 - 4) that we're working on
		       (bingo-row-iterator 0))
		   ; for each space-delimited number in the bingo entries
		   ; TODO: in our input, single-digit numbers are preceded by 
		   ; two spaces, rather than one. There's the delimiting space,
		   ; and the space where a number in the tens column would be.
		   ; I'm sure there's a better way to handle this but, for now,
		   ; we will just skip when bingo-number is an empty string.
		   (loop for bingo-number in (uiop:split-string line :separator " ")
			 do
			 (if (equal bingo-number "")() ; skip mis-parsed space
			   (progn
			     (setf (aref bingo-table bingo-table-row-index bingo-row-iterator) bingo-number)
			     (incf bingo-row-iterator))))
		 (incf bingo-table-row-index))))))))
	      

(print bingo-inputs)(terpri)
(print bingo-tables)(terpri)

