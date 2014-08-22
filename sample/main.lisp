
(load "src/init.lisp")

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
		   (write-sequence bytes out))))
