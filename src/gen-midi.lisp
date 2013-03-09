
;;; pitch is determined by
;;; :note 1 2 3 4 5 6 7
;;; :clef
;;; :scale C C#(Db) D D#(Eb) E F F#(Gb) G A A#(Bb) B
;;;        0 2      3 4      5 6 7      8 9 10     11
(defun simple-number-to-note (num)
  (assert (member-p num '(1 2 3 4 5 6 7)))
  (list :pitch (+ (getf *default-scale-pitch* :C)
                  (cdr (assoc num *major-note-offset*))
                  (* 12 0))
        :interval (/ 1 1)))

(defun note-char-to-num (note-char)
  (assert (member-p note-char '(#\1 #\2 #\3 #\4 #\5 #\6 #\7)))
  (- (char-code note-char)
     (char-code #\0)))

(defun music-note-to-midi-note (music-note scale major)
  (list :delta-time (getf music-note :delta-time)
        :pitch (+ (getf *default-scale-pitch* scale)
                  (cdr (assoc (note-char-to-num 
                               (getf music-note :note-char)) 
                              major))
                  (getf music-note :pitch-shift))
        :interval (getf music-note :interval-rate)))

(defun simple-music-note-to-midi-note (music-note)
  (music-note-to-midi-note 
   music-note :C *major-note-offset*))

(defun note-to-events (note)
  `#(,(make-note-on :delta-time (beat-to-tpqn (or (getf note :delta-time) 0))
                    :type :note-on
                    :channel 0
                    :note-number (getf note :pitch)
                    :velocity *def-on-velo*)
     ,(make-note-off :delta-time (beat-to-tpqn (getf note :interval))
                     :type :note-off
                     :channel 0
                     :note-number (getf note :pitch)
                     :velocity *def-off-velo*)))

;;; event to binary functions
(defun ctrl-event-to-bytes (event)
  (let ((type (getf event :type)))
    (assert (member-p type *ctrl-events*))
    `(,@(varlen-to-bytes (or (getf event :delta-time) 0))
      ,(+ (or (getf event :channel) 0) 
          (* #x10 (getf *ctrl-event-code* type)))
      ,@(mapcar #'(lambda (p) (getf event p))
                (getf *ctrl-event-params* type)))))

(defun sysex-event-to-bytes (event)
  (assert nil)
  nil)

(defun meta-event-to-bytes (event)
  (let ((meta-bytes (getf event :bytes)))
    `(,@(varlen-to-bytes (or (getf event :delta-time) 0))
      #xFF ,(getf *meta-event-code* (getf event :type))
      ,@(varlen-to-bytes (length meta-bytes))
      ,@(coerce meta-bytes 'list))))

(defun event-to-bytes (event)
  (let ((type (getf event :type)))
    (cond ((member-p type *ctrl-events*) (ctrl-event-to-bytes event))
          ((member-p type *meta-events*) (meta-event-to-bytes event))
          ((member-p type *sysex-events*) (sysex-event-to-bytes event)))))

;;; chunk to bytes function
(defun chunk-to-bytes (chunk)
  (let ((track-bytes (getf chunk :bytes)))
    (concatenate 'vector
                 (map 'vector #'char-code (getf chunk :magic))
                 (make-n-bytes 4 (length track-bytes))
                 track-bytes)))

;;; sequence to events function
(defun gen-sound-track (seq)
  `(,(make-program-change :acoustic-grand-piano)
    ,@(coerce (with-f-reduce-map #'note-to-events seq) 'list)
    ,(make-end-of-track)))

(defun gen-def-ctrl-track ()
  `(,(make-set-tempo *MPQN*)
    ;; numer (log denom/log2) metro 32nds
    ,(make-time-signature #x04 #x02 #x02 #x08)
    ,(make-end-of-track)))

;;; gen chunk from track
(defun head-to-chunk (format n-track division)
  (list :magic "MThd"
        :bytes (with-f-reduce-map #'(lambda (x) (make-n-bytes 2 x))
                                  (vector format n-track division))))

(defun track-to-chunk (track)
  (list :magic "MTrk"
        :bytes (with-f-reduce-map #'event-to-bytes track)))

;;; gen midi file function
(defun gen-midi-file-chunks (seqs)
  (let ((tracks (cons (gen-def-ctrl-track)
                      (mapcar #'gen-sound-track seqs))))
    (cons (head-to-chunk 1 (length tracks) *TPQN*)
          (map 'list #'track-to-chunk tracks))))

(defun gen-midi-file-bytes (seqs)
  (with-f-reduce-map #'chunk-to-bytes
                     (gen-midi-file-chunks seqs)))

