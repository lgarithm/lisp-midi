
;;;;
;;;; music constants
;;;;


;;; 0 1 2 3 4 5 6 7 8 9 10 11
;;; C   D   E F   G   A    B
(defparameter *major-note-offset*
  (pairlis '(1 2 3 4 5 6 7)
           '(0 2 4 5 7 9 11)))

(defun part-sum (lst)
  (labels ((rec (acc sum lst)
                (if (null lst)
                    (reverse acc)
                  (rec (cons sum acc) 
                       (+ sum (car lst)) 
                       (cdr lst)))))
          (rec '() 0 lst)))

(defparameter *scale-intervals*
  (list :major '(2 2 1 2 2 2 1)
        :natural-minor '(2 1 2 2 1 2 2)
        :harmonic-minor '(2 1 2 2 1 3 1)
        :melodic-minor '(2 1 2 2 2 2 1)))

(defparameter *mode-intervals*
  (list :ionian '(2 2 1 2 2 2 1)
        :dorian '(2 1 2 2 2 1 2)
        :phrygian '(1 2 2 2 1 2 2)
        :lydian '(2 2 2 1 2 2 1)
        :mixolydian '(2 2 1 2 2 1 2)
        :aeolian '(2 1 2 2 1 2 2)
        :locrian '(1 2 2 1 2 2 2)))


(defparameter *scale-offsets*
  (alist-to-plist
   (mapcar #'(lambda (pr) 
               (cons (car pr)
                     (part-sum (cdr pr))))
           (plist-to-alist *scale-intervals*))))

(defparameter *mode-offsets*
  (alist-to-plist
   (mapcar #'(lambda (pr)
               (cons (car pr)
                     (part-sum (cdr pr))))
           (plist-to-alist *mode-intervals*))))
