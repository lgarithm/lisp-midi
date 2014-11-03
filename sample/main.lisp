
(load "src/init.lisp")

;;; example below generates mid file from mdl definition
(defparameter *input* "sample/canon.mdl.txt")
(defparameter *output* "sample/ca.mid")
(format t "generate ~a from ~a~%" *output* *input*)
(compile-mdl-to-midi *input* *output*)
