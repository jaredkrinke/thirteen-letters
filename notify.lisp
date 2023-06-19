(in-package :13ls)

(defun-debounced run-notify-script (* 20 60) ()
  (spew "Running notify.sh...~%")
  (if (uiop:getenv "IFTTT_KEY")
      (progn
	(bt:make-thread #'(lambda ()
			    (uiop/run-program:run-program "./notify.sh")
			    :name "NotifyOneShot")))
      (spew "Note: No IFTTT_KEY!~%")))

(defun notify (&key (client nil))
  (if (not (and client
		(or (equal "Jared" (slot-value client 'name))
		    (slot-value client 'bot))))
      (run-notify-script)))
