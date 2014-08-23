
(defun assert-with-op (op expr expect)
  (format t "test with ~a " op)
  (let ((result (funcall op expr expect)))
    (format t "~a~%" (if result "OK" "FAIL"))
    result))

(defun test (expr)
  (format t "test ~s~%" expr)
  (format t "EVALS-TO: ~s~%" (eval expr)))

(defun test-with-op (op expr expect &optional (quiet nil))
  (if (null quiet)
      (format t "test-eq-with ~s ~s ~s~%" op expr expect))
  (let ((val (eval expr)))
    (if (funcall op val expect)
        (prog1 t (format t "OK~%"))
      (prog1 nil (format t "FAIL ~s EVALS-TO ~s~%" expr val)))))

(defun test-eq (expr expect)
  (test-with-op #'eq expr expect))




(load "src/init.lisp")

(defvar *all-tests*
  (list '("utils" . "test-utils.lisp")
        '("parse midi" . "test-parse-midi.lisp")
        '("parse mdl" . "test-parse-mdl.lisp")
        '("gen midi" . "test-gen-midi.lisp")))

(mapcar #'(lambda (test)
            (let ((name (car test))
                  (script (cdr test)))
              (format t "test ~a with ~a~%" name script)
              (load (concatenate 'string "test/" script))))
        *all-tests*)
