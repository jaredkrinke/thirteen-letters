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
    (defparameter *a* ((@ "a" char-code-at) 0))
    (defparameter *z* ((@ "z" char-code-at) 0))

    (defun debug (message)
      (let ((debug-div ((@ document get-element-by-id) "debug"))
	    (node ((@ document create-element) "p")))
	(append-text node message)
	((@ debug-div append-child) node)
	nil))

    (defun error (error-string)
      (debug (+ "ERROR: " error-string))
      nil)

    (defun get-letter-counts (word)
      (let ((counts (ps:create))
	    (code))
	(loop for i from 0 to 25 do (setf (elt counts i) 0))
	(loop for i from 0 to (1- (@ word length))
	      do (setf code ((@ word char-code-at) i))
		 (if (and (>= code *a*)
			  (<= code *z*))
		     (incf (elt counts (- code *a*)))
		     (return-from get-letter-counts nil)))
	counts))

    (defun is-valid (expected-counts word)
      (let ((actual-counts (get-letter-counts word)))
	(if (not actual-counts) (return-from is-valid nil))
	(loop for i from 0 to 25
	      do (if (> (elt actual-counts i) (elt expected-counts i))
		     (return-from is-valid nil)))
	t))

    (watch
     (let ((socket)
	   (scrambled)
	   (scrambled-letter-counts (get-letter-counts ""))
	   (timer-id)
	   (validation-timer-id)
	   (countdown-end-timestamp)
	   (intro-div (get-id "intro"))
	   (main-div (get-id "main"))
	   (time-div (get-id "time"))
	   (time-title-span (get-id "time-title"))
	   (time-left-span (get-id "time-left"))
	   (top-div (get-id "top"))
	   (player-count-span (get-id "player-count"))
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
	 (setf scrambled-letter-counts (get-letter-counts scrambled))
	 (show guess-input)
	 (show time-div)
	 (setf (@ time-title-span inner-text) "Time")
	 (hide result-div)
	 (setf (@ guess-input value) "")
	 ((@ guess-input focus))
	 (update-letters scrambled))
       
       (defun round-ended (message)
	 (show result-div)
	 (show time-div)
	 (setf (@ time-title-span inner-text) "Next")
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
	 (if countdown-end-timestamp
	     (let* ((remaining-ms (max 0 (- countdown-end-timestamp ((@ -date now)))))
		    (remaining-seconds (/ remaining-ms 1000))
		    (floored ((@ -math floor) remaining-seconds))
		    (text (+ "" floored))
		    (fraction-ms (max 0 ((@ -math floor) (* (- remaining-seconds
							       floored)
							    1000)))))
	       (setf (@ time-left-span inner-text) text)
	       (if (> remaining-ms 0)
		   (setf timer-id (set-timeout update-time-left fraction-ms)))))
	 nil)

       (defun update-remaining (remaining)
	 (setf countdown-end-timestamp (+ ((@ -date now))
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
		   ((@ tbody append-child) row)))))

	 ;;; Also update player count, if needed
	 (let ((client-count (@ message clients)))
	   (if (>= client-count 0)
	       (setf (@ player-count-span inner-text)
		     (max 1 client-count))))

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
	 (let* ((name-raw (@ name-input value))
		(name ((@ name-raw trim))))
	   (if name
	       (progn
		 (send (ps:create type "rename"
				  name name))
		 ((@ local-storage set-item) *name-key* name))))
	 nil)

       (defun handle-visibility-changed ()
	 (if (and (not socket)
		  (= (@ document visibility-state) "visible"))
	     (connect-socket))
	 nil)

       (defun connect-socket ()
	 (setf socket (ps:new (-web-socket *web-socket-url*)))
	 (setf (@ socket onopen) send-rename)
	 (setf (@ socket onmessage) handle-update)
	 (setf (@ socket onclose) #'(lambda () (setf socket nil))))

       (defun handle-start ()
	 (watch
	  (hide intro-div)
	  (show main-div)
	  (show top-div)
	  (if (not socket) (connect-socket))
	  ((@ document add-event-listener) "visibilitychange" #'(lambda ()
								  (set-timeout handle-visibility-changed 100)))
	  nil))

       (defun get-guess ()
	 ((@ ((@ (@ guess-input value) to-lower-case)) trim)))
       
       (defun send-guess ()
	 (let* ((word (get-guess))
		(message (ps:create type "guess"
				    word word)))
	   (send message)
	   (setf (@ guess-input value) "")
	   ((@ guess-input class-list remove) "invalid")))

       (defun validate-current-guess ()
	 (setf validation-timer-id nil)
	 (let ((word (get-guess)))
	   (if (is-valid scrambled-letter-counts word)
	       ((@ guess-input class-list remove) "invalid")
	       ((@ guess-input class-list add) "invalid")))
	 nil)

       (defun schedule-validation-if-needed ()
	 (if (not validation-timer-id)
	     (setf validation-timer-id (set-timeout validate-current-guess 50))))

       (defun handle-key-down (event)
	 (watch
	  (let ((key (@ event key)))
	    (if (= key "Enter")
		(progn (send-guess))
		(schedule-validation-if-needed)))))

       (defun handle-name-key-down (event)
	 (watch
	  (let ((key (@ event key)))
	    (if (= key "Enter") (handle-start)))))

       (setf (@ start-button onclick) handle-start)
       (setf (@ name-input onkeydown) handle-name-key-down)
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
     ("body" :background-color "#eee"
	     :color "#222")
     ("body, button, input" :font-family "'Cascadia Mono', 'Consolas', monospace")
     ("table" :width "100%")
     ("input" :font-size "100%")
     
     (".left" :text-align "left")
     (".center" :text-align "center")
     (".right" :text-align "right")
     (".caps" :text-transform "uppercase")
     (".invalid" :outline "solid 2px red")
     
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

Enter a name and click **Start** to get started.

During each round, a set of thirteen letters will be shown. Type in the longest word you can find and hit Enter/Return to submit your guess. You may continue guessing until the timer in the upper-right counts down to zero.")))

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
		(:table (:tbody :id "tbody"))
		(:p :class "center"
		    "(player count: "
		    (:span :id "player-count" "1")
		    ")"))
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
