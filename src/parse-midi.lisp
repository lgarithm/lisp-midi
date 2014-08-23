
;;; midi I/O funcitons

(defun parse-midi-division (division)
  (let ((type (ldb (byte 1 15) division)))
    (case type
          ((0) (list :type 0
                     :ticks-per-quater-note (ldb (byte 15 0) division)))
          ((1) (list :type 1
                     :negative-smpte-format (ldb (byte 7 8) division)
                     :ticks-per-frame (ldb (byte 8 0) division))))))

;;; midi event read functions

(defun take-midi-event (status-byte stream)
  (let* ((type (ldb (byte 4 4) status-byte))
         (channel (ldb (byte 4 0) status-byte))
         (type-argc (assoc type
                           (pairlis '(#x8 #x9 #xA #xB #xC #xD #xE) 
                                    '(2 2 2 2 1 1 2)))))
    (assert type-argc)
    (list :status-byte status-byte
          :type type
          :channel channel
          :parameters (read-n-from-stream (cdr type-argc) stream))))

(defun take-sysex-event (status-byte stream)
  (let* ((length (read-varlen-from-stream stream))
         (event-data (read-n-from-stream length stream)))
    (list :status-byte status-byte
          :length length
          :data event-data)))

(defun take-meta-event (status-byte stream)
  (let* ((type (read-my-stream stream))
         (length (read-varlen-from-stream stream))
         (event-data (read-n-from-stream length stream)))
    (list :status-byte status-byte
          :type type
          :length length
          :data event-data)))

(defun take-event (stream last-status-byte)
  (let ((delta-time (read-varlen-from-stream stream))
        (status-byte (try-my-stream stream)))
    (if (= 1 (ldb (byte 1 7) status-byte))
        (setf status-byte (read-my-stream stream))
      (setf status-byte last-status-byte))
    (assert (= 1 (ldb (byte 1 7) status-byte)))
    (let ((event 
           (cond ((= #xF0 status-byte) (take-sysex-event status-byte stream))
                 ((= #xF7 status-byte) (take-sysex-event status-byte stream))
                 ((= #xFF status-byte) (take-meta-event status-byte stream))
                 (t (take-midi-event status-byte stream)))))
      (cons :delta-time (cons delta-time event)))))

(defun parse-midi-events (data)
  (labels ((rec (acc stream last-status-byte)
                (if (empty-my-stream-p stream)
                    (reverse acc)
                  (let ((event (take-event stream last-status-byte)))
                    (rec (cons event acc) 
                         stream
                         (getf event :status-byte))))))
          (rec '() (make-my-stream data) 0)))

;;; midi block read functions 
(defun read-midi-head-trunk (in)
  (let ((magic (read-ascii-string in 4))
        (length (read-uint-n in 4)))
    (assert (string= "MThd" magic))
    (assert (= 6 length))
    (let ((format (read-uint-n in 2))
          (n-tracks (read-uint-n in 2))
          (division (read-uint-n in 2)))
      (assert (member-p format '(0 1 2)))
      (assert (if (= 0 format) (= 1 n-tracks) t))
      (list :magic magic
            :length length
            :format format
            :n-tracks n-tracks
            :division (parse-midi-division division)))))

(defun read-midi-sound-trunk (in)
  (let* ((magic (read-ascii-string in 4))
         (length (read-uint-n in 4))
         (data (read-raw-bytes in length))
         (events (parse-midi-events data)))
    (assert (string= "MTrk" magic))
    (list :magic magic
          :length length
;         :data data
          :events events)))

(defun read-midi-raw-trunk (in)
  (let* ((magic (read-ascii-string in 4))
         (length (read-uint-n in 4))
         (data (read-raw-bytes in length)))
    `(:magic ,magic :length ,length :data ,data)))

(defun read-midi-file (in)
  (let* ((hd (read-midi-head-trunk in))
         (body (replicate (getf hd :n-tracks)
                          #'(lambda () (read-midi-sound-trunk in)))))
    (cons hd body)))
