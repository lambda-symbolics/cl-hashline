(in-package #:cl-hashline)

;;;; -- Logical Line Encoding --

(defun line-anchor (line)
  "Return the first four uppercase hexadecimal SHA-256 digits of UTF-8 LINE."
  (let ((digest (ironclad:digest-sequence
                 ':sha256 (babel:string-to-octets line :encoding ':utf-8))))
    (format nil "~2,'0X~2,'0X" (aref digest 0) (aref digest 1))))

(defun line-ending (content)
  "Return CRLF if CONTENT contains CRLF, otherwise LF."
  (if (search (format nil "~C~C" #\Return #\Newline) content)
      (format nil "~C~C" #\Return #\Newline)
      (string #\Newline)))

(defun mixed-line-endings-p (content)
  "Return true when CONTENT contains both LF and CRLF line endings."
  (let ((crlf-p nil)
        (lf-p   nil))
    (loop for position = (position #\Newline content)
            then (position #\Newline content :start (1+ position))
          while position
          do (if (and (plusp position)
                      (char= (char content (1- position)) #\Return))
                 (setf crlf-p t)
                 (setf lf-p t)))
    (and crlf-p lf-p)))

(defun final-newline-p (content)
  "Return true when CONTENT ends in LF, including CRLF."
  (and (plusp (length content))
       (char= (char content (1- (length content))) #\Newline)))

(defun join-lines (lines line-ending final-newline-p)
  "Join logical LINES with LINE-ENDING and the requested terminal newline."
  (with-output-to-string (stream)
    (loop for line in lines
          for first-p = t then nil
          unless first-p do (write-string line-ending stream)
          do (write-string line stream))
    (when (and final-newline-p lines)
      (write-string line-ending stream))))
