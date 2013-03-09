
(pprint *ctrl-rates*)
; (pprint (get-ctrl-rate #\a))
; (pprint (get-ctrl-rate #\[))

(setf lines (get-all-file-lines-joined "sample/canon.mdl.txt"))
; (pprint lines)
(setf mdls (chars-to-mdls lines))
; (pprint mdls)
(pprint (mapcar #'(lambda (ss) (concatenate 'string ss)) mdls)) 

(setf mdl (car mdls))
; (pprint mdl)
; (exit)

(pprint (make-init-table *global-ctrl-chars*))
(pprint (make-init-table *local-ctrl-chars*))

(setf acc (list :notes '()
                :global-env (make-init-table *global-ctrl-chars*)
                :local-env (make-init-table *local-ctrl-chars*)))

(pprint acc)
(print (parse-mdl-char acc #\[))

(setf raw-notes (mdl-to-raw-notes mdl))
(pprint raw-notes)

(setf notes (parse-raw-notes raw-notes))
(pprint notes)
