
;;;; test parse-midi


; (defun read-all-midi-chunks (in)
  ; (let* ((hd (read-midi-head-trunk in)))
    ; (setf body '())
    ; (dotimes (i (getf hd :n-tracks))
      ; (print (read-midi-raw-trunk in)))))

;;(print (read-midi-file *input*))
;;(print (read-all-midi-chunks *input*))

(let ((exp1 (with-open-file (in "test/data/1234567+1.mid" :element-type '(unsigned-byte 8))
                            (read-midi-file in)))
      (exp2 (with-open-file (in "test/data/1234567+1.lisp")
                            (read in))))
  (assert-with-op #'equalp exp1 exp2))
