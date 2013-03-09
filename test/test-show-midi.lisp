
;;;; test show-midi


; (defun read-all-midi-chunks (in)
  ; (let* ((hd (read-midi-head-trunk in)))
    ; (setf body '())
    ; (dotimes (i (getf hd :n-tracks))
      ; (print (read-midi-raw-trunk in)))))



(defvar *file-name* "data/ca.mid")
(defvar *input* (open *file-name* :element-type '(unsigned-byte 8)))
                                        ;(print (read-midi-file *input*))
; (print (read-all-midi-chunks *input*))
(print (read-midi-file *input*))
(close *input*)
