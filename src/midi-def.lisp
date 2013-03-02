
;;; events definitions
(defparameter *ctrl-events*
  (list :note-off #x8
        :note-on #x9
        :note-aftertouch #xA
        :controller #xB
        :program-change #xC
        :channal-aftertouch #xD
        :pitch-bend #xE))

(defparameter *sysex-events*
  (list :normal
        :divided-0 ;
        :divided-1 ; last one
        :authorization))

(defparameter *meta-events*
  (list :end-of-track '(:type 47 :length 0)
        :set-tempo '(:type 81 :length 3)
        :time-signature '(:type 88 :length 4)))

;;; event constructors
(defmacro make-note-on (&rest body)
  `(list ,@body))

(defmacro make-note-off (&rest body)
  `(list ,@body))

(defmacro make-program-change (&rest body)
  `(list ,@body))

(defmacro make-end-of-track ()
  `(list :type :end-of-track
         :bytes #()))

(defmacro make-set-tempo (tempo)
  `(list :type :set-tempo
         :bytes ,(make-n-bytes 3 (eval tempo))))

(defmacro make-time-signature (a b c d)
  `(list :type :time-signature
         :bytes ,(vector a b c d)))

(defparameter *music-instrument-number*
  (list :piano 0
        :guitar 24
        :violin 40))

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

;;; 0 1 2 3 4 5 6 7 8 9 10 11
;;; C   D   E F   G   A    B
(defparameter *major-note-offset*
  (pairlis '(1 2 3 4 5 6 7)
           '(0 2 4 5 7 9 11)))

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

;;; mdl definitions

(defparameter *default-num-pitch* #(60 62 64 65 67 69 71))

(defparameter *interval-ctrl-chars* "[]_^")
(defparameter *pitch-ctrl-chars* "<>-+!?b#")
(defparameter *ctrl-rates*
  (pairlis (coerce "[]_^<>-+!?b#" 'list)
           '(-1 1 -1 1 -12 12 -12 12 -1 1 -1 1)))

(defparameter *global-ctrl-chars* "[]<>?!")
(defparameter *local-ctrl-chars* "+-#b^_.")
(defparameter *mdl-note-chars* "s01234567")

(defparameter *mdl-chars* "<>!?[]-+b#_^.$s01234567")