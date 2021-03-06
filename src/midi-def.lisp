
;;;;
;;;; default values
;;;;

;;; event constructors
(defmacro make-end-of-track ()
  `(list :type :end-of-track
         :bytes #()))

(defmacro make-set-tempo (tempo)
  `(list :type :set-tempo
         :bytes ,(make-n-bytes 3 (eval tempo))))

(defmacro make-time-signature (a b c d)
  `(list :type :time-signature
         :bytes ,(vector a b c d)))

;;; constant definitions
(defparameter *default-scale-pitch*
  (list :C 60
        :C# 61 :Db 61
        :D 62
        :D# 63 :Eb 63
        :E 64
        :F 65
        :F# 66 :Gb 66
        :G 67
        :G# 68 :Ab 68
        :A 69
        :A# 70 :Bb 70
        :B 71))

(defparameter *microseconds-per-minute* 60000000)
; (defparameter *BPM* 120)
(defparameter *BPM* 56)
; (defparameter *MPQN* (/ *microseconds-per-minute* *BPM*))
(defparameter *MPQN* (truncate (/ *microseconds-per-minute* *BPM*)))
(defparameter *TPQN* 960) ; *ticks-per-quater-note*

(defun beat-to-tpqn (beat)
  (* *TPQN* beat))

;;; default parameters

(defparameter *def-on-velo* #x64)
(defparameter *def-off-velo* #x0)
