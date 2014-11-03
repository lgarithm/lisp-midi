
(defun chars-to-mdls (line)
  (split #\$ (remove-if-not
              #'(lambda (x) (member-p x *mdl-chars*))
              line)))

(defun make-raw-note (global-env local-env mdc)
  (let ((ctb (append global-env local-env)))
    (labels ((get-tot-ctrl-value
              (ctrl-chars)
              (apply #'+ (map 'list #'(lambda (cc)
                                        (* (getf ctb cc)
                                           (cdr (assoc cc *ctrl-rates*))))
                              ctrl-chars))))
            (list :interval-rate (* (expt 2 (get-tot-ctrl-value *interval-ctrl-chars*))
                                    (- 2 (/ 1 (expt 2 (getf ctb #\.)))))
                  :pitch-shift (get-tot-ctrl-value *pitch-ctrl-chars*)
                  :note-char mdc))))

(defun parse-mdl-char (acc mdc)
  (cond ((member-p mdc *global-ctrl-chars*)
         (progn
           (incf (getf (getf acc :global-env) mdc))
           acc))
        ((member-p mdc *local-ctrl-chars*)
         (progn
           (incf (getf (getf acc :local-env) mdc))
           acc))
        ((member-p mdc *mdl-note-chars*)
         (progn
           (setf (getf acc :raw-notes)
                 (cons (make-raw-note (getf acc :global-env)
                                      (getf acc :local-env) mdc)
                       (getf acc :raw-notes)))
           (setf (getf acc :local-env)
                 (make-init-table *local-ctrl-chars*))
           acc))))

(defun mdl-to-raw-notes (mdl)
  (reverse
   (getf (reduce
          #'parse-mdl-char mdl
          :initial-value (list
                          :raw-notes '()
                          :global-env (make-init-table *global-ctrl-chars*)
                          :local-env (make-init-table *local-ctrl-chars*)))
         :raw-notes)))

(defun parse-raw-notes (raw-notes)
  (labels
   ((rec (acc pause-acc raw-notes)
         (if (null raw-notes)
             (reverse acc)
           (let* ((raw-note (car raw-notes))
                  (note-char (getf raw-note :note-char))
                  (interval-rate (getf raw-note :interval-rate)))
             (cond ((member-p note-char "0s")
                    (rec acc
                         (+ pause-acc interval-rate)
                         (cdr raw-notes)))
                   ((member-p note-char "1234567")
                    (rec (cons (list :delta-time pause-acc
                                     :interval-rate interval-rate
                                     :pitch-shift (getf raw-note :pitch-shift)
                                     :note-char note-char)
                               acc)
                         0
                         (cdr raw-notes))))))))
   (rec '() 0 raw-notes)))

(defun compile-mdls-to-bytes (mdls)
  (let* ((raw-notess (mapcar #'mdl-to-raw-notes mdls))
         (music-notess (mapcar #'parse-raw-notes raw-notess))
         (seqs (mapcar #'(lambda (music-notes)
                           (mapcar #'simple-music-note-to-midi-note
                                   music-notes))
                       music-notess))
         (tracks (cons (gen-def-ctrl-track)
                       (mapcar #'gen-sound-track seqs)))
         (file-bytes (gen-midi-file-bytes seqs))
         (bytes (map 'vector #'make-byte file-bytes)))
    bytes))

(defun compile-mdl-to-midi (input-file-name output-file-name)
  (write-raw-bytes-to-file output-file-name
                           (chaincall #'compile-mdls-to-bytes
                                      #'chars-to-mdls
                                      #'get-all-file-lines-joined
                                      input-file-name)))
