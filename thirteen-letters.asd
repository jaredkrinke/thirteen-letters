(defsystem #:thirteen-letters
  :depends-on (#:lparallel)
  :serial t
  :components ((:file "thirteen-letters"))
  :build-operation program-op
  :build-pathname "thirteen-letters"
  :entry-point "thirteen-letters:menu")
