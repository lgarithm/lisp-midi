
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
  (music-note-to-midi-note music-note 
                           :C
                           *major-note-offset*))

(defun note-to-events (note)
  (vector (make-note-on :delta-time (beat-to-tpqn (or (getf note :delta-time) 0))
                        :type :note-on
                        :channel 0
                        :pitch (getf note :pitch)
                        :velocity *def-on-velo*)
          (make-note-off :delta-time (beat-to-tpqn (getf note :interval))
                         :type :note-off
                         :channel 0
                         :pitch (getf note :pitch)
                         :velocity *def-off-velo*)))

;;; event to binary functions
(defun ctrl-event-to-bytes (event)
  (let ((delta-time (getf event :delta-time))
        (type (getf event :type))
        (channel (getf event :channel)))
    (assert (some (lambda (x) (eql x type)) '(:note-on :note-off)))
    (concatenate 'vector
                 (varlen-to-bytes delta-time)
                 (vector (+ channel (* #x10 (getf *ctrl-events* type))))
                 (case type
                       ((:note-on) (vector (getf event :pitch)
                                           (getf event :velocity)))
                       ((:note-off) (vector (getf event :pitch)
                                            (getf event :velocity)))))))

(defun sysex-event-to-bytes (event)
  (assert nil)
  nil)

(defun meta-event-to-bytes (event)
  (let* ((delta-time (or (getf event :delta-time) 0))
         (meta-type (getf event :type))
         (meta-bytes (getf event :bytes))
         (meta-length (length meta-bytes))
         (meta-def (getf  *meta-events* meta-type)))
    (assert meta-def)
    (assert (= meta-length (getf meta-def :length)))
    (concatenate 'vector
                 (varlen-to-bytes delta-time)
                 (vector #xFF (getf meta-def :type))
                 (varlen-to-bytes meta-length)
                 meta-bytes)))

(defun event-to-bytes (event)
  (let ((type (getf event :type)))
    (cond ((getf *ctrl-events* type) (ctrl-event-to-bytes event))
          ((getf *meta-events* type) (meta-event-to-bytes event))
          ((getf *sysex-events* type) (sysex-event-to-bytes event)))))

;;; chunk to bytes function
(defun chunk-to-bytes (chunk)
  (let* ((magic-bytes (map 'vector #'char-code (getf chunk :magic)))
         (track-bytes (getf chunk :bytes))
         (track-length (length track-bytes)))
    (concatenate 'vector
                 magic-bytes
                 (make-n-bytes 4 track-length)
                 track-bytes)))

;;; sequence to events function
(defun add-end-of-track-event (events)
  (concatenate 'vector events (vector (make-end-of-track))))

(defun gen-sound-track (seq)
  (add-end-of-track-event
   (with-f-reduce-map #'note-to-events seq)))

(defun gen-def-ctrl-track ()
  (add-end-of-track-event
   (list (make-set-tempo *MPQN*)
         ;; numer (log denom/log2) metro 32nds
         (make-time-signature #x04 #x02 #x02 #x08))))

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
