
(defparameter *lib-path* "src")

(defun test (expr)
  (format t "test ~a~%" expr)
  (format t "EVALS-TO: ~a~%" (eval expr)))

(defun test-with-op (op expr expect)
  (format t "test-eq-with ~a ~a ~a~%" op expr expect)
  (let ((val (eval expr)))
    (if (funcall op val expect)
	(prog1 t (format t "OK~%"))
      (prog1 nil (format t "FAIL ~a EVALS-TO ~a~%" expr val)))))

(defun test-eq (expr expect)
  (test-with-op #'eq expr expect))

(setf deps '("base.lisp"
             "midi-def.lisp"
             "show-midi.lisp"
             "gen-midi.lisp"
             "parse-mdl.lisp"))

(mapcar #'(lambda (file) 
            (format t "loading ~a/~a~%" *lib-path* file)
            (load (concatenate 'string *lib-path* "/" file))) 
        deps)

(format t "~%~a~%" "test base")
(load "test/test-base.lisp")

(format t "~%~a~%" "test show-midi")
(load "test/test-show-midi.lisp")

(format t "~%~a~%" "test parse-mdl")
(load "test/test-parse-mdl.lisp")

(format t "~%~a~%" "test gen-midi")
(load "test/test-gen-midi.lisp")