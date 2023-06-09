(defpackage :13l-server
  (:documentation "Web server for online, multiplayer Thirteen Letters game")
  (:nicknames :13ls)
  (:use :cl
	:13l)
  (:local-nicknames (:lp :lparallel)
		    (:sp :spinneret))
  (:export #:start-server
	  #:stop-server))

(in-package :13ls)

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

(defmacro loop-over-lines ((line-var path) &body body)
  "Run LOOP over each line of a file"
  `(with-open-file (stream ,path) ; TODO: gensym!
     (loop for ,line-var = (read-line stream nil)
	   while ,line-var
	   ,@body)))

(defun get-time ()
  "Return a timestamp (in seconds, as a ratio)"
  (/ (get-internal-real-time) internal-time-units-per-second))

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
(defvar *news* nil)
(defvar *stats* (make-hash-table :test 'equal))

(defun message->json (message)
  "Encodes MESSAGE as JSON"
  (json:encode-json-to-string message))

(defun send (client message)
  "Sends MESSAGE to CLIENT, if non-nil"
  (if message
      (let ((text (message->json message)))
	(spew "Sending to ~a: ~a~%" client text)
	(hunchensocket:send-text-message client text))))

(defun broadcast (message)
  "Broadcasts a message to all players, if non-nil"
  (if message
      (let ((text (message->json message)))
	(spew "Broadcast: ~a~%" text)
	(loop for client in (hunchensocket:clients *socket*)
	      do (hunchensocket:send-text-message client text)))))

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

(defun get-current-news ()
  "Gets current news as a message, if any"
  (if *news* (list (cons :type :news)
		   (cons :fragment *news*))))

(defun queue-news-update (news)
  "Queues a task to update *NEWS* and broadcast the update"
  (lp:submit-task *queue* #'(lambda ()
			      (setf *news* news)
			      (broadcast (get-current-news)))))

(defun client-connect (client)
  "Handles a client connection work item"
  (spew "Client connected: ~a~%" client)
  (push client *clients*)
  (send client (get-current-state))
  (send client (get-current-news)))

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

(defun update-stats ()
  "Updates overall win statistics"
  (let ((winner (first *leaderboard*)))
    (if winner
	(with-slots (client) winner
	  (with-slots (name) client
	    (if name (setf (gethash name *stats*) (1+ (or (gethash name *stats*) 0)))))))))

(defun format-stats ()
  "Format *STATS* into an HTML string"
  (let ((pairs nil)
	(*print-pretty* nil)
	(sp:*html-style* :tree))
    (maphash #'(lambda (k v) (pushnew (cons k v) pairs)) *stats*)
    (setf pairs (sort pairs #'(lambda (a b) (> (cdr a) (cdr b)))))
    (sp:with-html-string
      (:h3 "Hall of Fame")
      (:table (:tr (:th :class "center" "Name") (:th :class "center" "Wins"))
	      (loop for pair in pairs
		    for i from 1 to 5
		    do (:tr (:td (car pair)) (:td :class "right" (cdr pair))))))))

(defun round-end ()
  "Broadcasts the results of the round that just ended"
  (setf *round-done* t)
  (broadcast-state)
  (update-stats)
  (if *leaderboard*
      (queue-news-update (format-stats))))

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