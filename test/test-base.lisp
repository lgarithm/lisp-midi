
(test-with-op #'equalp '(replicate 5 (lambda () 1)) '(1 1 1 1 1))
(setf stream (make-my-stream '(1 2 3 4 5)))
(test-eq '(read-my-stream stream) 1)
(test-eq '(read-my-stream stream) 2)
(test-eq '(read-my-stream stream) 3)
(test-eq '(read-my-stream stream) 4)
(test-eq '(empty-my-stream-p stream) nil)
(test-eq '(read-my-stream stream) 5)
(test-eq '(empty-my-stream-p stream) t)

(test-eq '(member-p #\s "123123") nil)
(test-eq '(member-p #\s "123s123") t)

(test-with-op #'equalp '(make-init-table "abcd") '(#\a 0 #\b 0 #\c 0 #\d 0))

(test-with-op #'equalp '(make-n-bytes 2 256) #(1 0))
(test-with-op #'equalp '(make-n-bytes 3 256) #(0 1 0))
(test-with-op #'equalp '(make-n-bytes 2 257) #(1 1))
(test-with-op #'equalp '(make-n-bytes 3 257) #(0 1 1))


(test-with-op #'equalp 
              '(plist-to-alist '(:a 1 :b 2))
              '((:a . 1) (:b . 2)))

(test-with-op #'equalp 
              '(alist-to-plist '((1 . 2) (3 . 4)))
              '(1 2 3 4))

