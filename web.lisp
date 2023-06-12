(defpackage :13l-web
  (:documentation "HTML/JS/CSS generator for Thirteen Letters on the web")
  (:nicknames :13lw)
  (:use :cl)
  (:import-from #:parenscript #:ps #:@)
  (:local-nicknames (#:sp #:spinneret)))

(in-package :13lw)

;;; JavaScript
;;; TODO: Path should be shared with back-end code
(defvar *web-socket-url* "wss://api.schemescape.com/ws/13l")
(defvar *web-socket-url-override* nil)

(ps:defpsmacro get-id (id)
  `((@ document get-element-by-id) ,id))

(ps:defpsmacro watch (&body body)
  `(ps:try (progn ,@body) (:catch (e) (error (+ (@ e message) " (" (@ e line-number) ")")))))

(ps:defpsmacro clear-children (node)
  `(setf (@ ,node inner-h-t-m-l) ""))

(ps:defpsmacro append-text (node text)
  `((@ ,node append-child) ((@ document create-text-node) ,text)))

(defun make-script (&key web-socket-url)
  (ps
    (defparameter *web-socket-url* (ps:lisp web-socket-url))
    (defparameter *name-key* "13l_name")
    (defun debug (message)
      (let ((debug-div ((@ document get-element-by-id) "debug"))
	    (node ((@ document create-element) "p")))
	(append-text node message)
	((@ debug-div append-child) node)
	nil))
    (defun error (error-string)
      (debug (+ "ERROR: " error-string))
      nil)

    (watch
     (let ((socket)
	   (scrambled)
	   (timer-id)
	   (round-end-timestamp)
	   (intro-div (get-id "intro"))
	   (main-div (get-id "main"))
	   (time-div (get-id "time"))
	   (time-left-span (get-id "time-left"))
	   (top-div (get-id "top"))
	   (tbody (get-id "tbody"))
	   (result-div (get-id "result"))
	   (debug-div (get-id "debug"))
	   (news-div (get-id "news"))
	   (name-input (get-id "name"))
	   (start-button (get-id "start"))
	   (guess-input (get-id "guess"))
	   (winner-span (get-id "winner"))
	   (winning-word-span (get-id "winning-word")))
       (defun hide (element) ((@ element class-list add) "hidden"))
       (defun show (element)((@ element class-list remove) "hidden"))
       
       (defun update-letters (word)
	 (dotimes (i (@ word length))
	   (let ((letter-span (get-id (+ "l" i))))
	     (setf (@ letter-span inner-text) (aref word i)))))
       
       (defun round-started (message)
	 (setf scrambled (@ message scrambled))
	 (show guess-input)
	 (show time-div)
	 (hide result-div)
	 (setf (@ guess-input value) "")
	 ((@ guess-input focus))
	 (update-letters scrambled))
       
       (defun round-ended (message)
	 (show result-div)
	 (hide time-div)
	 (hide guess-input)
	 (let* ((entries (@ message leaderboard))
		(winning-entry (and entries (aref entries 0))))
	   (update-letters (@ message solution))
	   (setf (@ winner-span inner-text) (if winning-entry (@ winning-entry name) "(no one)"))
	   (setf (@ winning-word-span inner-text) (if winning-entry (@ winning-entry word) "---"))))

       (defun unregister-timer ()
	 (if timer-id (clear-timeout timer-id)))

       (defun reset-remaining ()
	 (unregister-timer)
	 (setf (@ time-left-span inner-text) "")
	 nil)

       (defun update-time-left ()
	 (reset-remaining)
	 (if round-end-timestamp
	     (let* ((remaining-ms (max 0 (- round-end-timestamp ((@ -date now)))))
		    (remaining-seconds (/ remaining-ms 1000))
		    (floored ((@ -math floor) remaining-seconds))
		    (text (+ "" floored))
		    (fraction-ms (max 0 ((@ -math floor) (* (- remaining-seconds
								      floored)
								   1000)))))
	       (setf (@ time-left-span inner-text) text)
	       (setf timer-id (set-timeout update-time-left fraction-ms))))
	 nil)

       (defun update-remaining (remaining)
	 (setf round-end-timestamp (+ ((@ -date now))
				      ((@ -math floor) (* remaining 1000))))
	 (update-time-left)
	 nil)
       
       (defun update-leaderboard (message)
	 ;;; Update leaderboard
	 (let ((entries (@ message leaderboard)))
	   (clear-children tbody)
	   (if entries
	       (dolist (entry entries)
		 (let ((row ((@ document create-element) "tr"))
		       (col-name ((@ document create-element) "td"))
		       (col-word ((@ document create-element) "td")))
		   (append-text col-name (@ entry name))
		   ((@ row append-child) col-name)
		   (append-text col-word (@ entry word))
		   ((@ col-word class-list add) "right")
		   ((@ col-word class-list add) "caps")
		   ((@ row append-child) col-word)
		   ((@ tbody append-child) row)))
	       (let ((row ((@ document create-element) "tr"))
		     (col ((@ document create-element) "td")))
		 ((@ col set-attribute) "colspan" 2)
		 ((@ col class-list add) "center")
		 (append-text col "(empty)")
		 ((@ row append-child) col)
		 ((@ tbody append-child) row))))

	 ;;; Also update time, if needed
	 (let ((remaining (@ message remaining)))
	   (if remaining
	       (update-remaining remaining)
	       (reset-remaining))))
       
       (defun handle-error ()
	 (error "WebSocket error!"))

       (defun update-news (message)
	 (let ((fragment (@ message fragment)))
	   (setf (@ news-div inner-h-t-m-l) fragment)))
       
       (defun handle-update (event)
	 (watch
	  (let* ((json (@ event data))
		 (message ((@ *JSON* parse) json)))
	    (case (@ message type)
	      ("state"
	       (if (not (= scrambled (@ message scrambled))) (round-started message))
	       (update-leaderboard message))
	      ("result"
	       (update-leaderboard message)
	       (round-ended message))
	      ("news"
	       (update-news message)))
	    nil)))
       
       (defun send (message)
	 (if socket
	     (let ((json ((@ *json* stringify) message)))
	       ((@ socket send) json))))
       
       (defun send-rename ()
	 (let* ((nameRaw (@ name-input value))
		(name ((@ nameRaw trim))))
	   (if name
	       (progn
		 (send (ps:create type "rename"
				  name name))
		 ((@ local-storage set-item) *name-key* name))))
	 nil)

       (defun handle-start ()
	 (watch
	  (hide intro-div)
	  (show main-div)
	  (show top-div)
	  (if (not socket)
	      (progn (setf socket (ps:new (-web-socket *web-socket-url*)))
		     (setf (@ socket onopen) send-rename)
		     (setf (@ socket onmessage) handle-update)))
	  nil))
       
       (defun send-guess ()
	 (let* ((word-raw (@ guess-input value))
		(word ((@ word-raw to-lower-case)))
		(message (ps:create type "guess"
				    word word)))
	   (send message)
	   (setf (@ guess-input value) "")))
       
       (defun handle-key-down (event)
	 (watch
	  (let ((key (@ event key)))
	    (if (= key "Enter")
		(progn (send-guess))))))

       (setf (@ start-button onclick) handle-start)
       (setf (@ start-button onerror) handle-error)
       (setf (@ guess-input onkeydown) handle-key-down)

       ;;; Try to restore name from localStorage
       (let ((saved-name ((@ local-storage get-item) *name-key*) ""))
	 (if saved-name (setf (@ name-input value) saved-name)))
       nil))))

;;; CSS
(defparameter *css*
  (cl-css:css
   '(("html, body" :margin 0)
     ("body, button, input" :font-family "'Cascadia Mono', 'Consolas', monospace")
     ("table" :width "100%")
     ("input" :font-size "100%")
     
     (".left" :text-align "left")
     (".center" :text-align "center")
     (".right" :text-align "right")
     (".caps" :text-transform "uppercase")
     
     (".main" :display "flex"
	      :flex-direction "column"
	      :align-items "center")
     (".time" :display "flex"
	      :flex-direction "column"
	      :align-items "center"
	      :position "absolute"
	      :padding "0.25em"
	      :top 0
	      :right 0)
     (".top" :width "100%"
	     :display "flex"
	     :flex-direction "column"
	     :align-items "center"
	     :margin-top "1em")
     ("#root" :display "flex"
	      :flex-direction "column"
	      :align-items "center"
	      :max-width "40em"
	      :margin "auto"
	      :padding "1em")
     ("#guess, #result" :font-size "150%"
			:margin-top "1em")
     ("#start" :margin "1em"
	       :padding "0.25em 1em"
	       :font-size "150%"
	       :font-weight "bold")
     ("#letters" :margin "1em"
		 :text-align "center")
     
     ("#letters > div" :display "inline-block"
                       :padding "0.125em"
                       :font-weight "bold"
                       :font-size "150%"
		       :text-transform "uppercase")
     ("#time-title" :font-weight "bold")
     ("#time-left" :font-size "125%")
     (".time > p" :padding "0.125em"
		  :margin 0)
     (.invisible :display "hidden")
     (.hidden :display "none"))))

;;; HTML
(defparameter *intro* (sp:with-html-string ("**Thirteen Letters** is a game where players compete to find the longest word that can be constructed from the given set of letters.

It's also a game that needs better documentation!")))

(defmacro page ((&key title) &body body)
  "Outputs an HTML page"
  `(sp:with-html
     (:doctype)
     (:html
      (:head
       (:title ,title)
       (:meta :name "viewport"
	      :content "width=device-width, initial-scale=1, shrink-to-fit=no"))
      (:style (:raw *css*))
      (:body ,@body))))

(defun make-index-html (&key web-socket-url)
  "Outputs root HTML page"
  (page (:title "(thirteen-letters)")
    (:div :id "root"
	  (:h1 "(thirteen-letters)")
	  (:div :id "intro"
		(:h2 "Welcome!")
		(:raw *intro*)
		(:p "Name: " (:input :id "name" :type "text"))
		(:p :class "center"
		    (:button :id "start" "Start")))
	  (:div :id "main" :class "hidden main"
		(:div :id "letters"
		      (dotimes (n 13)
			(:div :id (format nil "l~d" n) "?")))
		(:input :id "guess" :type "text"))
	  (:div :id "time" :class "hidden time"
		(:p :id "time-title" "Time")
		(:p :id "time-left"))
	  (:div :id "result" :class "hidden"
		"Winner: "
		(:span :id "winner")
		" ("
		(:span :id "winning-word" :class "caps")
		")")
	  (:div :id "top" :class "top hidden"
		(:h3 :class "center" "Leaderboard")
		(:table (:tbody :id "tbody")))
	  (:div :id "news" :class "top")
	  (:div :id "debug"))
    (:script (:raw (make-script :web-socket-url web-socket-url)))))


;;; Generate index.html
(defun write-index-html (&key (web-socket-url (or *web-socket-url-override* *web-socket-url*)))
  (setf sp:*suppress-inserted-spaces* t)
  (let ((*print-pretty* nil)
	(sp:*html-style* :tree))
    (with-open-file (out "index.html" :direction :output
				      :if-exists :supersede)
      (setf sp:*html* out)
      (make-index-html :web-socket-url web-socket-url))
    (format t "Wrote index.html with WebSocket URL: ~a~%" web-socket-url)))

(write-index-html)
