
(let ((bytes1 (chaincall #'compile-mdls-to-bytes
			 #'chars-to-mdls
			 #'get-all-file-lines-joined
			 "test/data/1234567+1.txt"))
      (bytes2 (coerce (load-raw-bytes "test/data/1234567+1.mid") 'vector)))
  (test-with-op #'equalp bytes1 bytes2 t))


(pprint *ctrl-rates*)
; (pprint (get-ctrl-rate #\a))
; (pprint (get-ctrl-rate #\[))

(defvar lines (get-all-file-lines-joined "sample/canon.mdl.txt"))
(defvar mdls (chars-to-mdls lines))

(pprint (mapcar #'(lambda (ss) (concatenate 'string ss)) mdls))

(defvar mdl (car mdls))

(pprint (make-init-table *global-ctrl-chars*))
(pprint (make-init-table *local-ctrl-chars*))

(defvar acc (list :notes '()
                :global-env (make-init-table *global-ctrl-chars*)
                :local-env (make-init-table *local-ctrl-chars*)))

(pprint acc)
(print (parse-mdl-char acc #\[))

(defvar raw-notes (mdl-to-raw-notes mdl))
(pprint raw-notes)

(defvar notes (parse-raw-notes raw-notes))
(pprint notes)
