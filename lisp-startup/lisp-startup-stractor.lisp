;;; Expose Swank for outside hacking with Emacs.
(ql:quickload :swank)
(setf swank::*loopback-interface* "0.0.0.0")
(swank:create-server :style swank:*communication-style* :port 4005 :style :spawn :dont-close t)

;;; Load our code.
(push "/lisp/" ql:*local-project-directories*)
(ql:quickload :speechtractor)
