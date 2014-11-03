
;;;;
;;;; midi events definitions
;;;;


;;; midi channel events
(defparameter *ctrl-events*
  (list :note-off
        :note-on
        :note-aftertouch
        :controller
        :program-change
        :channal-aftertouch
        :pitch-bend))

(defparameter *ctrl-event-code*
  (list :note-off #x8
        :note-on #x9
        :note-aftertouch #xA
        :controller #xB
        :program-change #xC
        :channal-aftertouch #xD
        :pitch-bend #xE))

(defparameter *ctrl-event-params*
  (list :note-off '(:note-number :velocity)
        :note-on '(:note-number :velocity)
        :note-aftertouch '(:note-number :aftertouch-value)
        :controller '(:controller-number :controller-value)
        :program-change '(:program-number)
        :channel-aftertouch '(:aftertouch-value)
        :pitch-bend '(:pitch-value-lsb :pitch-value-msb)))

;;; midi meta events
(defparameter *meta-events*
  (list :sequence-number
        :text-event
        :copyright-notice
        :track-name
        :instrument-name
        :lyrics
        :marker
        :cue-point
        :midi-channel-prefix
        :end-of-track
        :set-tempo
        :smpte-offset
        :time-signature
        :key-signature
        :sequencer-specific))

(defparameter *meta-event-code*
  (list :sequence-number 0
        :text-event 1
        :copyright-notice 2
        :track-name 3
        :instrument-name 4
        :lyrics 5
        :marker 6
        :cue-point 7
        :midi-channel-prefix 32
        :end-of-track 47
        :set-tempo 81
        :smpte-offset 84
        :time-signature 88
        :key-signature 89
        :sequencer-specific 127))

(defparameter *meta-event-lengths*
  (list :sequence-number 2
        :midi-channel-prefix 1
        :end-of-track 0
        :set-tempo 3
        :smpte-offset 5
        :time-signature 4
        :key-signature 2))

;;; midi system ex events
(defparameter *sysex-events*
  (list :normal
        :divided-0 ;
        :divided-1 ; last one
        :authorization))

