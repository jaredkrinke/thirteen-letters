(defun-debounced notify (* 30 60) ()
  (uiop/run-program:run-program "./notify.sh"))
