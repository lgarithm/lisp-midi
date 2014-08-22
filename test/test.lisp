

(defun test (expr)
  (format t "test ~s~%" expr)
  (format t "EVALS-TO: ~s~%" (eval expr)))

(defun test-with-op (op expr expect)
  (format t "test-eq-with ~s ~s ~s~%" op expr expect)
  (let ((val (eval expr)))
    (if (funcall op val expect)
        (prog1 t (format t "OK~%"))
      (prog1 nil (format t "FAIL ~s EVALS-TO ~s~%" expr val)))))

(defun test-eq (expr expect)
  (test-with-op #'eq expr expect))

(load "src/init.lisp")

(format t "~%~a~%" "test utils")
(load "test/test-utils.lisp")

(format t "~%~a~%" "test show-midi")
(load "test/test-show-midi.lisp")

(format t "~%~a~%" "test parse-mdl")
(load "test/test-parse-mdl.lisp")

(format t "~%~a~%" "test gen-midi")
(load "test/test-gen-midi.lisp")
