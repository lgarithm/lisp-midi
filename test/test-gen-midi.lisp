
(setf melody '(1 2 3 4 5 6 7))

(test '(mapcar #'simple-number-to-note melody))

(test '(gen-sound-track (mapcar #'simple-number-to-note melody)))

(test '(mapcar #'event-to-bytes
	       (gen-sound-track 
		(mapcar #'simple-number-to-note 
			'(1 2 3 4 5 6 7)))))
