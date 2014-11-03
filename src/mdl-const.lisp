
;;;;
;;;; music definition language
;;;;

;;; mdl definitions
(defparameter *default-num-pitch*
  #(60 62 64 65 67 69 71))

(defparameter *interval-ctrl-chars* "[]_^")
(defparameter *pitch-ctrl-chars* "<>-+!?b#")
(defparameter *ctrl-rates*
  (pairlis (coerce "[]_^<>-+!?b#" 'list)
           '(-1 1 -1 1 -12 12 -12 12 -1 1 -1 1)))

(defparameter *global-ctrl-chars* "[]<>?!")
(defparameter *local-ctrl-chars* "+-#b^_.")
(defparameter *mdl-note-chars* "s01234567")

(defparameter *mdl-chars* "<>!?[]-+b#_^.$s01234567")
