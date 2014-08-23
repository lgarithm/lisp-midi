

;;; generic functions

(defun replicate (n exp)
  (let ((lst (make-array n)))
    (dotimes (i n)
      (setf (elt lst i) (funcall exp)))
    (coerce lst 'list)))

(defun with-f-reduce-map (f lst)
  (reduce #'(lambda (x y) (concatenate 'vector x y))
          (map 'vector f lst) :initial-value #()))

(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        #'(lambda (&rest args)
            (reduce #'funcall fns
                    :from-end t
                    :initial-value (apply fn1 args))))
    #'identity))

(defun chaincall (&rest expr)
  (funcall (apply #'compose (butlast expr))
           (car (last expr))))

(defun tie1st (fun arg)
  (lambda (&rest rest)
    (apply fun arg rest)))

(defun member-p (ele st)
  (some #'(lambda (x) (eq ele x)) st))

(defun make-init-table (chars)
  (coerce (with-f-reduce-map
           #'(lambda (x) (list x 0))
           (coerce chars 'list)) 'list))


;;; binary operation functions

(defun make-uint (lst)
  (reduce #'(lambda (x y) (+ (* 256 x) y)) lst :initial-value 0))

(defun make-byte (number)
  (ldb (byte 8 0) number))

(defun make-n-bytes (n number)
  (let ((vec (make-array n)))
    (dotimes (i n)
      (setf (elt vec i)
            (ldb (byte 8 (* 8 i)) number)))
    (reverse vec)))


;;; binary I/O functions

(defun read-raw-bytes (in n)
  (replicate n #'(lambda () (read-byte in))))

(defun read-ascii-string (in n)
  (map 'string #'code-char
       (replicate n #'(lambda () (read-byte in)))))

(defun read-uint-n (in n)
  (make-uint (replicate n #'(lambda () (read-byte in)))))

(defun load-raw-bytes (file-name)
  (with-open-file (in file-name
		      :element-type '(unsigned-byte 8))
		  (read-raw-bytes in (file-length in))))

(defun write-raw-bytes-to-file (file-name bytes)
  (with-open-file (out file-name
                       :direction :output
                       :if-exists :supersede
                       :element-type '(unsigned-byte 8))
                  (with-standard-io-syntax
                   (write-sequence bytes out))))

;;; file I/O functions

(defun get-all-file-lines-joined (file-name)
  (with-open-file
   (input file-name)
   (labels ((rec (acc)
                 (let ((line (read-line input nil)))
                   (if line (rec (cons line acc))
                     (eval `(concatenate 'string ,@(reverse acc)))))))
           (rec '()))))


;;; my stream ADT functions

(defun make-my-stream (lst)
  (let ((lst (coerce lst 'list)))
    (list (lambda ()
	    (let ((first (car lst)))
	      (setf lst (cdr lst))
	      first))
          (lambda () (null lst))
          (lambda () (car lst)))))

(defun empty-my-stream-p (stream)
  (funcall (cadr stream)))

(defun read-my-stream (stream)
  (funcall (car stream)))

(defun try-my-stream (stream)
  (funcall (caddr stream)))

(defun read-n-from-stream (n stream)
  (replicate n #'(lambda () (read-my-stream stream))))


;;; variable length functios

(defun read-varlen-from-stream (stream)
  (labels ((rec (acc stream)
                (let* ((next-byte (read-my-stream stream))
                       (lo (ldb (byte 7 0) next-byte))
                       (acc (+ lo (* 128 acc))))
                  (if (= 1 (ldb (byte 1 7) next-byte))
                      (rec acc stream) acc))))
          (rec 0 stream)))

(defun varlen-to-bytes (number)
  (labels ((rec1 (acc number)
                 (if (= 0 number)
                     (reverse acc)
                   (let ((lo (ldb (byte 7 0) number))
                         (hi (ash number -7)))
                     (rec1 (cons lo acc) hi))))
           (rec2 (acc lst)
                 (if (null lst)
                     acc
                   (rec2 (cons (+ 128 (car lst)) acc)
                         (cdr lst)))))
          (let ((lst (rec1 (list (ldb (byte 7 0) number))
                           (ash number -7))))
            (rec2 (list (car lst)) (cdr lst)))))


;;; list functions

(defun split (sep lst)
  (let ((stream (make-my-stream lst)))
    (labels ((take-one-sub (acc)
                           (if (empty-my-stream-p stream)
                               (reverse acc)
                             (let ((next (read-my-stream stream)))
                               (if (eql sep next)
                                   (reverse acc)
                                 (take-one-sub (cons next acc))))))
             (take-all-subs (acc)
                            (if (empty-my-stream-p stream)
                                (reverse acc)
                              (let ((next (take-one-sub '())))
                                (take-all-subs (cons next acc))))))
            (remove-if #'null (take-all-subs '())))))

(defun alist-to-plist (alist)
  (labels ((rec (acc res)
                (if (null res)
                    (reverse acc)
                  (rec (cons (cdar res)
                             (cons (caar res) acc))
                       (cdr res)))))
          (rec '() alist)))

(defun plist-to-alist (plist)
  (labels ((rec (acc res)
                (if (null res)
                    (reverse acc)
                  (rec (cons (cons (car res)
                                   (cadr res))
                             acc)
                       (cddr res)))))
          (rec '() plist)))
