(defpackage :13l-front-end
  (:documentation "HTML/JS/CSS generator for Thirteen Letters")
  (:nicknames :13lf)
  (:use :cl)
  (:import-from #:parenscript #:ps)
  (:local-nicknames (#:sp #:spinneret)))

(in-package :13lf)

;;; JavaScript
(defparameter *web-socket-uri* "ws://127.0.0.1:13131/ws/13l")

(defparameter *intro* (sp:with-html-string ("**Thirteen Letters** is game where players compete to find the longest word that can be constructed from the given set of letters.

It's also a game that needs better documentation!!! I guess...")))

(ps:defpsmacro TODO (message)
  `(debug ,message))

(ps:defpsmacro get-id (id)
  `((ps:chain document get-element-by-id) ,id))

(defparameter *script*
  (ps (defparameter *web-socket-uri* (ps:lisp *web-socket-uri*))
    (defun debug (message)
      (let ((debug-div ((ps:chain document get-element-by-id) "debug"))
	    (node ((ps:chain document create-element) "p"))
	    (text ((ps:chain document create-text-node) message)))
	((ps:chain node append-child) text)
	((ps:chain debug-div append-child) node)
	nil))
    (defun error (event) (debug (ps:chain event 'message)))

    (debug "Here we go...")
    (setf (ps:chain window onerror) error)
    
    (let ((state "intro")
	  (socket)
	  (intro-div (get-id "intro"))
	  (main-div (get-id "main"))
	  (top-div (get-id "top"))
	  (result-div (get-id "result"))
	  (debug-div (get-id "debug"))
	  (name-input (get-id "name"))
	  (start-button (get-id "start"))
	  (guess-input (get-id "guess"))
	  (winner-span (get-id "winner")))
      (defun update (event)
	(let* ((json (ps:chain event data))
	       (message ((ps:chain *JSON* parse) json)))
	  (case (ps:chain message type)
	    ("state" (TODO "state"))
	    ("result" (TODO "state")))))
      (defun start ()
	(debug "Starting...")
	(let ((name (ps:chain name-input value)))
	  (if (not socket)
	      (progn (setf socket (ps:new (-web-socket *web-socket-uri*)))
		     (setf (ps:chain socket onmessage) update)))
	  nil))
      (setf (ps:chain start-button onclick) start)
      (setf (ps:chain start-button onerror) error)
      (debug "Ready."))))

;;; HTML
(defmacro page ((&key title) &body body)
  "Outputs an HTML page"
  `(sp:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title))
      (:body ,@body))))

(defun make-index-html ()
  "Outputs root HTML page"
  (page (:title "(thirteen-letters)")
    (:h1 "(thirteen-letters)")
    (:div :id "intro"
	  (:h2 "Welcome!")
	  (:raw *intro*)
	  (:input :id "name" :type "text")
	  (:br)
	  (:button :id "start" "Start"))
    (:div :id "main"
	  (:div :id "letters"
		(dotimes (n 13)
		  (:span :id (format nil "l~d" n) "A")))
	  (:input :id "guess" :type "text"))
    (:div :id "top"
	  (:h3 "Leaderboard")
	  (:table (:thead (:th (:td "Name") (:td "Score")))
		  (:tbody)))
    (:div :id "result"
	  (:h2 "Results")
	  (:p :id "result" "The winner is " (:span :id "winner") "!"))
    (:div :id "debug"
	  (:h2 "Debug log"))
    (:script (:raw *script*))))


;;; Generate index.html
(setf sp:*suppress-inserted-spaces* t)

(with-open-file (out "index.html" :direction :output
				  :if-exists :supersede)
  (setf sp:*html* out)
  (make-index-html))
