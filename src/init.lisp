
;;;;
;;;; load all
;;;;

(defparameter *lib-path* "src")
(setf deps '("base.lisp"
             "midi-def.lisp"
             "midi-events.lisp"
             "midi-const.lisp"
	     "mdl-const.lisp"
             "music-const.lisp"
             "show-midi.lisp"
             "gen-midi.lisp"
             "parse-mdl.lisp"))

(defun load-all (files)
  (mapcar #'(lambda (file)
	      (format t "loading ~a/~a~%" *lib-path* file)
	      (load (concatenate 'string *lib-path* "/" file)))
	  files))

(load-all deps)

