(defpackage :thirteen-letters
  (:documentation "Thirteen-letter word scramble game")
  (:nicknames :13l)
  (:use :cl)
  (:local-nicknames (:lp :lparallel))
  (:export #:play
	   #:menu))

(in-package :13l)

;;; General utility functions
(defvar *verbose* nil)

(defmacro spew (&rest args)
  "Logs to the console, if *verbose* is non-nil"
  `(and *verbose* (format t ,@args)))

(defun alistp (value)
  "Tests to see if a value is an a-list"
  (and (listp value)
       (every #'consp value)))

(defun alist-path (object &rest keys)
  "Walk down an AList path"
  (labels ((alist-path-recursive (object keys)
	     (if keys
		 (alist-path-recursive (cdr (assoc (car keys) object)) (cdr keys))
		 object)))
    (alist-path-recursive object keys)))

(defun get-time ()
  "Return a timestamp (in seconds, as a ratio)"
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun get-random (sequence)
  "Randomly selects an item from SEQUENCE"
  (nth (random (length sequence)) sequence))

(defmacro loop-over-lines ((line-var path) &body body)
  "Run LOOP over each line of a file"
  `(with-open-file (stream ,path) ; TODO: gensym!
     (loop for ,line-var = (read-line stream nil)
	   while ,line-var
	   ,@body)))

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

;;; The actual game
(defparameter *puzzle-length* 13)
(defparameter *difficulty-buckets* 10)

;;; TODO: Don't inject the word list at compile time since we're not compiling ahead of time anymore
(defmacro define-bucketed-words ()
  "Injects word list at compile time"
  (with-open-file (stream "data.lisp")
    `(defparameter *bucketed-words* ',(read stream))))

(define-bucketed-words)

(defvar *valid-words*
  (let ((hash-table (make-hash-table :test 'equal :size 264097)))
    (loop-over-lines (word "yawl/yawl-0.3.2.03/word.list")
      do (setf (gethash word hash-table) t))
    hash-table))

(defun only-letters-p (word)
  "Returns non-nil if the word consists solely of the letters A through Z"
  (let ((min1 (char-code #\a))
	(max1 (char-code #\z))
	(min2 (char-code #\A))
	(max2 (char-code #\Z)))
    (loop for character across word
	  do (let ((code (char-code character)))
	       (if (not (or (and (>= code min1) (<= code max1))
			    (and (>= code min2) (<= code max2))))
		   (return-from only-letters-p nil))))
    t))

(defun get-letter-counts (word)
  "Gets an array of the letter counts within a word"
  (let ((normalized-word (string-downcase word))
	(base (char-code #\a))
	(counts (make-array 26)))
    (loop for letter across normalized-word
	  do (incf (aref counts (- (char-code letter) base))))
    counts))

(defun fully-scrambled-p (scrambled solution)
  "Returns NIL if any character in SCRAMBLED matches a letter in SOLUTION"
  (loop for scrambled-character across scrambled
	for solution-character across solution
	when (char-equal scrambled-character solution-character) do (return nil)
	finally (return t)))

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

(defun play (&optional (difficulty 0))
  "Play one round on the given difficulty level"
  (let* ((solution (get-random (nth difficulty *bucketed-words*)))
	 (scrambled (scramble solution)))
    (loop for tries upfrom 0
	  for attempt upfrom 1
	  with continue = t
	  with won = nil
	  with preamble = "Unscramble the following word"
	  while continue
	  do (format t
		     "~%~%~a, or enter 'q' to give up (attempt #~a):~%  ~a~a~%  ~a~%~%> "
		     preamble
		     attempt
		     (char-repeat #\Space tries)
		     (char-repeat #\_ (- *puzzle-length* tries))
		     scrambled)
	     (let* ((guess (read-line)))
	       (cond ((string= guess solution)
		      (format t
			      "~%~%Correct! Solved on attempt ~a (difficulty ~a).~%~%"
			      attempt
			      (1+ difficulty))
		      (setf continue nil)
		      (setf won t))
		     ((string= guess "q")
		      (format t "~%~%Better luck next time!~%~%")
		      (setf continue nil))
		     ((string= scrambled solution)
		      (format t "~%~%Too slow! Better luck next time!~%~%")
		      (setf continue nil))
		     (t
		      (unscramble scrambled solution tries)
		      (cond ((string= scrambled solution)
			     (format t "~%~%Too slow! The correct answer was: ~a~%~%" solution)
			     (setf continue nil))
			    (t
			     (format t "~%~%Nope! Guess again"))))))
	  finally (return (and won tries)))))

(defun menu ()
  "Show the title menu and prompt for difficulty level (or exit)"
  (format t
	  "
=== Thirteen Letters ===

 - Goal: Unscramble a thirteen-letter word in as few guesses as possible.
 - After each incorrect guess, one letter will be unscrambled.
 - There are ~a difficulty levels (1 being the easiest)~%~%"
	  *difficulty-buckets*)

  (loop with continue = t
	while continue
	do (format t
		   "Enter a difficulty level, 1 - ~a (or 'q' to quit):~%~%> "
		   *difficulty-buckets*)
	   (let* ((input (read-line))
		  (number (parse-integer input :junk-allowed t))
		  (difficulty (and number (>= number 1) (<= number *difficulty-buckets*) number)))
	     (cond ((string= "q" input)
		    (format t "~%~%So long!~%")
		    (setf continue nil))
		   (difficulty
		    (play (1- difficulty)))
		   (t
		    (format t "~%Invalid input!~%"))))))

;;; TODO
(defun TODO (&optional note)
  (format t "TODO: ~a~%" note))
  ;(error "TODO: fix!"))

;;; Leaderboard entries
(defclass entry ()
  ((client :initarg :client)
   (word :initarg :word)
   (time :initform (get-time))))

(defgeneric entry-word-length (entry))

(defmethod entry-word-length ((entry entry))
  (length (slot-value entry 'word)))

(defmethod print-object ((object entry) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (client word time) object
      (format stream "~a: ~s (~f)" client word time))))

;;; TODO: Seems like a macro might make sense...
(defun entry< (a b)
  (let ((a-length (entry-word-length a))
	(b-length (entry-word-length b)))
    (or (> a-length b-length)
	(and (= a-length b-length)
	     (< (slot-value a 'time) (slot-value b 'time))))))

;;; Server

;;; WebSocket server
(defparameter *message-handlers*
  (list (cons "guess" 'client-guess)
	(cons "rename" 'client-rename)))

(defvar *clients* nil)

(defclass socket (hunchensocket:websocket-resource)
  ()
  (:documentation "WebSocket server/resource/socket")
  (:default-initargs :client-class 'client))

(defclass client (hunchensocket:websocket-client)
  ((name :initform "(unknown)"))
  (:documentation "WebSocket client"))

(defmethod hunchensocket:client-connected ((socket socket) client)
  (lp:submit-task *queue* 'client-connect client))

(defmethod hunchensocket:client-disconnected ((socket socket) client)
  (lp:submit-task *queue* 'client-disconnect client))

(defmethod hunchensocket:text-message-received ((socket socket) client text)
  ;;; TODO: There's probably a macro for this sort of non-nil chaining...
  (let* ((message (handler-case (json:decode-json-from-string text) (t () nil)))
	 (type (and (alistp message) (alist-path message :type)))
	 (handler (and type (cdr (assoc type *message-handlers* :test #'string-equal)))))
    (if handler
	(lp:submit-task *queue* handler client message)
	(spew "Couldn't find a handler for message: ~s~%" text))))

(defun find-websocket-handler (request)
  "Finds any relevant WebSocket handler, based on the REQUEST's SCRIPT-PATH"
  (if (equal (hunchentoot:script-name request) *socket-path*)
      *socket*))

;;; TODO: Same port as HTTP? If so, rename
(defparameter *socket-port* 13131)
(defparameter *socket-path* "/ws/13l" "Path to WebSocket interface")

(defvar *socket* (make-instance 'socket))
(defvar *server* (make-instance 'hunchensocket:websocket-acceptor
				:address "127.0.0.1"
				:port *socket-port*))

;;; TODO: Plus or star?
(defparameter *round-time* 60 "Length of each round (in seconds)")
(defparameter *intermission-time* 15 "Length of time between rounds (in seconds)")

(defvar *round-done* t "True if there is no active round")
(defvar *done* t "True if the server should stop")
(defvar *queue* nil "Task queue")

;;; Server main worker logic
(defvar *solution* nil)
(defvar *solution-letters* nil)
(defvar *scrambled* nil)
(defvar *round-end* nil)
(defvar *leaderboard* nil)

(defun message->json (message)
  "Encodes MESSAGE as JSON"

(defun send (client message)
  "Sends MESSAGE to CLIENT"
  (let ((text (message->json message)))
    (spew "Sending to ~a: ~a~%" client text)
    (hunchensocket:send-text-message client text)))
  (json:encode-json-to-string message))

(defun broadcast (message)
  "Broadcasts a message to all players"
  (let ((text (message->json message)))
    (spew "Broadcast: ~a~%" text)
    (loop for client in (hunchensocket:clients *socket*)
	  do (hunchensocket:send-text-message client text))))

(defun make-message (type object)
  "Makes a message of type TYPE"
  (cons (cons :type type)
	object))

(defun leaderboard->alist (&key reveal)
  "Formats *LEADERBOARD* entries as a-lists (revealing words if REVEAL is non-nil)"
  ;;; TODO: Consider just going straight to JSON?
  (mapcar #'(lambda (entry)
	      (with-slots (client word) entry
		(list (cons :name (slot-value client 'name))
		      (cons :word (if reveal word
				      (char-repeat #\? (length word)))))))
	  *leaderboard*))

(defun get-current-state ()
  "Gets the current puzzle state (in-progress or results)"
  (if *round-done*
      (list (cons :type :result)
	    (cons :solution *solution*)
	    (cons :leaderboard (leaderboard->alist :reveal t)))
      (list (cons :type :state)
	    (cons :scrambled *scrambled*)
	    (cons :remaining (float (max 0 (/ (- *round-end* (get-internal-real-time))
					      internal-time-units-per-second))))
	    (cons :leaderboard (leaderboard->alist :reveal nil)))))

(defun broadcast-state ()
  "Broadcasts puzzle state to all players"
  (broadcast (get-current-state)))

(defun client-connect (client)
  "Handles a client connection work item"
  (spew "Client connected: ~a~%" client)
  (push client *clients*)
  (send client (get-current-state)))

(defun client-disconnect (client)
  "Handles a client disconnection work item"
  (spew "Client disconnected: ~a~%" client)
  (setf *clients* (delete-if #'(lambda (c) (eql c client)) *clients*)))

(defun client-rename (client message)
  "Handles a work item that associates a name with the given client"
  (let ((name (alist-path message :name)))
    (cond (name
	   (setf (slot-value client 'name) name)
	   (spew "Renamed ~a to ~s~%" client (slot-value client 'name))))))

(defun client-guess (client message)
  "Handles a guess work item from the given client"
  (cond ((not *round-done*)
	 (let ((word (alist-path message :word)))
	   (if (and word
		    (valid-word-p word)
		    (update-leaderboard client word))
	       (broadcast-state))))
	(t (spew "Guess arrived after round ended~%"))))

(defun round-start (&optional (difficulty 0))
  "Selects a new puzzle and broadcasts to all players"
  (setf *solution* (get-random (nth difficulty *bucketed-words*)))
  (setf *solution-letters* (get-letter-counts *solution*))
  (setf *scrambled* (scramble *solution*))
  (setf *round-end* (+ (get-internal-real-time) (* *round-time* internal-time-units-per-second)))
  (setf *leaderboard* nil)
  (setf *round-done* nil)
  (broadcast-state))

(defun round-end ()
  "Broadcasts the results of the round that just ended"
  (setf *round-done* t)
  (broadcast-state))

(defun run-round ()
  "Runs a single round"
  (lp:submit-task *queue* 'round-start)
  (sleep *round-time*)
  (lp:submit-task *queue* 'round-end)
  (sleep *intermission-time*))

(defun valid-word-p (word)
  "Returns true if WORD is a real word and uses letters from *SCRAMBLED*"
  (and (only-letters-p word)
       (gethash word *valid-words*)
       (every #'<= (get-letter-counts word) *solution-letters*)))

(defun update-leaderboard (client word)
  "Updates the leaderboard (note: WORD is assumed valid); returns non-nil if the leaderboard was modified"
  (let ((existing-entry (find-if #'(lambda (e) (eql client (slot-value e 'client))) *leaderboard*))
	(should-update t))
    (if existing-entry
	(if (< (length (slot-value existing-entry 'word)) (length word))
	    (setf (slot-value existing-entry 'word) word)
	    (setf should-update nil))
	(setf *leaderboard* (cons (make-instance 'entry
						 :client client
						 :word word)
				  *leaderboard*)))
    (if should-update (setf *leaderboard* (stable-sort *leaderboard* #'entry<)))
    should-update))

(defun start-server ()
  "Runs the server"
  (setf *done* nil)
  (setf lp:*kernel* (lp:make-kernel 1))
  (setf *queue* (lp:make-channel))

  (setf hunchensocket:*websocket-dispatch-table* (list 'find-websocket-handler))
  (hunchentoot:start *server*)

  (spew "Server started")
  (loop while (not *done*)
	do (run-round))
  
  (hunchentoot:stop *server*)
  (setf hunchensocket:*websocket-dispatch-table* nil)
  
  (lp:end-kernel :wait t)
  (setf *queue* nil)
  (spew "Server stopped"))

(defun stop-server ()
  "Stops the server"
  (setf *done* t))
