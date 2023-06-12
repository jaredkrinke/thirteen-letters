(defpackage :13l-shared
  (:documentation "Package for sharing code amongst Thirteen Letters console, server, and web versions")
  (:nicknames :13l)
  (:use :cl)
  (:export #:*bucketed-words*
	   #:*difficulty-buckets*
	   #:*puzzle-length*
	   #:char-repeat
	   #:get-random
	   #:initialize-random-state
	   #:shuffle
	   #:shuffle-string
	   #:scramble
	   #:unscramble))

(in-package :13l)

(defparameter *puzzle-length* 13)
(defparameter *difficulty-buckets* 10)

(defmacro define-bucketed-words ()
  "Injects word list at compile time"
  (with-open-file (stream "data.lisp")
    `(defparameter *bucketed-words* ',(read stream))))

(define-bucketed-words)

(defun initialize-random-state ()
  "Initializes the random number generator"
  (setf *random-state* (make-random-state t)))

(defun get-random (sequence)
  "Randomly selects an item from SEQUENCE"
  (nth (random (length sequence)) sequence))

(defun fully-scrambled-p (scrambled solution)
  "Returns NIL if any character in SCRAMBLED matches a letter in SOLUTION"
  (loop for scrambled-character across scrambled
	for solution-character across solution
	when (char-equal scrambled-character solution-character) do (return nil)
	finally (return t)))

(defun shuffle (sequence &rest rest &key &allow-other-keys)
  "Returns a copy of SEQUENCE (as an array) with its elements shuffled (note: extra arguments are passed to MAKE-ARRAY)"
  (let* ((length (length sequence))
	 (array (apply #'make-array length :initial-contents sequence rest)))
    (loop for i upfrom 0
	  for n downfrom length
	  while (> n 1)
	  do (let ((target (+ i (random n))))
	       (rotatef (aref array i) (aref array target))))
    array))

(defun shuffle-string (string)
  "Returns a copy of STRING with its characters shuffled"
  (shuffle string :element-type 'character))

(defun char-repeat (character times)
  "Returns a string with CHARACTER repeated TIMES times"
  (make-array times :element-type 'character :initial-element character))

(defun scramble (word)
  "Shuffles a word, ensuring that no character is in the correct place"
  (loop with scrambled = word
	until (fully-scrambled-p scrambled word)
	do (setf scrambled (shuffle-string scrambled))
	finally (return scrambled)))

(defun unscramble (scrambled solution index)
  "Unscrambles the letter at INDEX in SCRAMBLED"
  (let* ((c (aref solution index))
	 (target (position c scrambled :start index)))
    (rotatef (aref scrambled index) (aref scrambled target))))
