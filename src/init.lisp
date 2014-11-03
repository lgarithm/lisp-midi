;;;;
;;;; load all libraries
;;;;

(defparameter *lib-path* "src")

(defparameter *deps* '("utils.lisp"
                       "midi-def.lisp"
                       "midi-events.lisp"
                       "midi-const.lisp"
                       "mdl-const.lisp"
                       "music-const.lisp"
                       "parse-midi.lisp"
                       "gen-midi.lisp"
                       "parse-mdl.lisp"))

(defun load-all (lib-path files)
  (mapcar #'(lambda (file)
	      (let ((file-path (concatenate 'string lib-path "/" file)))
		(format t "loading ~a~%" file-path)
		(load file-path)))
	  files))

(load-all *lib-path* *deps*)
