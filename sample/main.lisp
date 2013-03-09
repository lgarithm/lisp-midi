
(defparameter *lib-path* "src")
(setf deps '("base.lisp"
             "midi-def.lisp"
             "midi-events.lisp"
             "midi-const.lisp"
             "show-midi.lisp"
             "gen-midi.lisp"
             "parse-mdl.lisp"))

(defun load-all (files)
  (mapcar #'(lambda (file)
	      (format t "loading ~a/~a~%" *lib-path* file)
	      (load (concatenate 'string *lib-path* "/" file)))
	  files))



(load-all deps)

;;; example below generates mid file from mdl definition
(defparameter *input* "sample/canon.mdl.txt")
(defparameter *output* "sample/ca.mid")
(format t "generate ~a from ~a~%" *output* *input*)
(let* ((lines (get-all-file-lines-joined *input*))
       (mdls (chars-to-mdls lines))
       (bytes (compile-mdls-to-bytes mdls)))
  (with-open-file (out *output*
		       :direction :output
		       :if-exists :supersede
		       :element-type '(unsigned-byte 8))
		  (with-standard-io-syntax
		   (write-byte-sequence bytes out))))

