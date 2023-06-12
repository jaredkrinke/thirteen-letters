(defun-debounced notify (* 30 60) ()
  (bt:make-thread #'(lambda ()
		      (uiop/run-program:run-program "./notify.sh"))
		  :name "NotifyOneShot"))
