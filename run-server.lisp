(ql:quickload :13l-server)
(load "notify.lisp")

(in-package :13ls)

(setf *on-activity* 'notify)
(start-server-thread)
