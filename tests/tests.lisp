(in-package #:cl-hashline)

(fiveam:def-suite hashline-tests)

(fiveam:in-suite hashline-tests)

(defun test--split-lines (text)
  "Split LF or CRLF TEXT without manufacturing a terminal empty line."
  (coerce
   (loop with start = 0
         for end = (position #\Newline text :start start)
         while (< start (length text))
         collect (subseq text start
                         (if (and end (> end start)
                                  (char= (char text (1- end)) #\Return))
                             (1- end)
                             (or end (length text))))
         while end
         do (setf start (1+ end)))
   'vector))

(defun test--text (&rest lines)
  "Join LINES with LF separators and no final newline."
  (format nil "~{~A~^~%~}" lines))

(defun test--visible (&rest ranges)
  "Construct visibility from half-open one-based RANGES."
  (structlisp:make-integer-interval-set :intervals ranges))

(defun test--edit (text operations &optional (visible nil visible-p))
  "Edit TEXT with OPERATIONS under VISIBLE ranges."
  (edit-text text operations
             :visible-lines (if visible-p
                                visible
                                (test--visible
                                 (cons 1 (1+ (length (test--split-lines text))))))
             :split-lines   #'test--split-lines))

(fiveam:test anchors-and-line-conventions
  (fiveam:is (string= "E3B0" (line-anchor "")))
  (fiveam:is (string= "BA78" (line-anchor "abc")))
  ;; SHA-256 of UTF-8 U+00E9 begins 4a99557e.
  (fiveam:is (string= "4A99" (line-anchor (string (code-char #xE9)))))
  (dolist (ending (list (string #\Newline) (format nil "~C~C" #\Return #\Newline)))
    (dolist (terminal '(nil t))
      (let ((text (join-lines '("a" "" "b") ending terminal)))
        (fiveam:is (string= ending (line-ending text)))
        (fiveam:is (eq terminal (final-newline-p text)))
        (fiveam:is (not (mixed-line-endings-p text)))
        (fiveam:is (equalp #("a" "" "b") (test--split-lines text))))))
  (fiveam:is (string= "" (join-lines nil (string #\Newline) t)))
  (fiveam:is (not (final-newline-p "")))
  (fiveam:is (mixed-line-endings-p (format nil "a~%b~C~C" #\Return #\Newline))))

(fiveam:test edit-operation-table
  (let ((text (test--text "a" "b" "c")))
    (dolist (case '(((:op :replace-lines :start-line 2 :end-line 2 :content "x")
                     ("a" "x" "c") :replace 2 2 ("x"))
                    ((:op :delete-lines :start-line 2 :end-line 2)
                     ("a" "c") :delete 2 2 nil)
                    ((:op :replace-lines :start-line 2 :end-line 2 :content "")
                     ("a" "c") :replace 2 2 nil)
                    ((:op :insert-before :line 2 :content "x")
                     ("a" "x" "b" "c") :insert-before 2 2 ("x"))
                    ((:op :insert-after :line 2 :content "x")
                     ("a" "b" "x" "c") :insert-after 2 2 ("x"))))
      (destructuring-bind (operation expected kind start end lines) case
        (multiple-value-bind (result normalized) (test--edit text (list operation))
          (fiveam:is (string= (apply #'test--text expected) result))
          (fiveam:is (= 1 (length normalized)))
          (let ((entry (first normalized)))
            (fiveam:is (eq kind (getf entry :kind)))
            (fiveam:is (= start (getf entry :start)))
            (fiveam:is (= end (getf entry :end)))
            (fiveam:is (equal lines (getf entry :lines)))
            (fiveam:is (stringp (getf entry :summary)))))))))

(fiveam:test original-coordinates-and-newlines
  (dolist (ending (list (string #\Newline) (format nil "~C~C" #\Return #\Newline)))
    (dolist (terminal '(nil t))
      (let ((text (join-lines '("a" "b" "c" "d") ending terminal)))
        (fiveam:is
         (string= (join-lines '("a" "x" "c" "z") ending terminal)
                  (test--edit text
                              '((:op :replace-lines :start-line 4 :end-line 4 :content "z")
                                (:op :replace-lines :start-line 2 :end-line 2 :content "x")))))
        (fiveam:is (string= "" (test--edit text '((:op :delete-lines :start-line 1 :end-line 4))))))))
  (fiveam:is (string= (test--text "a" "x" "y" "c")
                     (test--edit (test--text "a" "b" "c")
                                 (list (list :op ':replace-lines :start-line 2 :end-line 2
                                             :content (format nil "x~C~Cy~C~C" #\Return #\Newline #\Return #\Newline)))))))

(fiveam:test visibility-is-complete-and-half-open
  (let ((text (test--text "a" "b" "c" "d")))
    (fiveam:signals unseen-lines
      (test--edit text '((:op :delete-lines :start-line 1 :end-line 3))
                  (test--visible '(1 . 2) '(3 . 5))))
    (fiveam:signals unseen-lines
      (test--edit text '((:op :insert-after :line 2 :content "x"))
                  (test--visible '(1 . 2))))
    (fiveam:signals unseen-lines
      (test--edit text '((:op :delete-lines :start-line 1 :end-line 1)) nil))
    (fiveam:is (string= "d" (test--edit text '((:op :delete-lines :start-line 1 :end-line 3))
                                      (test--visible '(1 . 2) '(2 . 4)))))))

(fiveam:test operation-validation
  (dolist (operation '((:op :unknown)
                       (:op :delete-lines :start-line 1)
                       (:op :delete-lines :start-line 0 :end-line 1)
                       (:op :delete-lines :start-line 2 :end-line 1)
                       (:op :delete-lines :start-line 1 :end-line 1 :extra t)
                       (:op :delete-lines :start-line 1 :start-line 2 :end-line 2)
                       (:op :replace-lines :start-line 1 :end-line 1 :content 42)
                       (:op :insert-before :line 1 :content "")
                       (:op :insert-after :line "1" :content "x")
                       (:op :replace-lines :start-line 1 :end-line 1 :content)
                       (:op :replace-empty :content "x")))
    (fiveam:signals invalid-operation
      (test--edit (test--text "a" "b") (list operation))))
  (fiveam:signals hashline-error
    (test--edit "a" '((:op :insert-before :line 1 :anchor "ZZZZ" :content "x")))))

(fiveam:test empty-resource
  (dolist (content (list "x" (format nil "x~%") (format nil "x~C~C" #\Return #\Newline)))
    (multiple-value-bind (result normalized)
        (test--edit "" (list (list :op ':replace-empty :content content)) nil)
      (fiveam:is (string= content result))
      (fiveam:is (eq ':replace-empty (getf (first normalized) :kind)))
      (fiveam:is (string= (line-ending content) (getf (first normalized) :line-ending)))
      (fiveam:is (eq (final-newline-p content) (getf (first normalized) :final-newline-p)))))
  (fiveam:signals invalid-operation
    (test--edit "" '((:op :replace-empty :content "a") (:op :replace-empty :content "b")) nil))
  (fiveam:signals unseen-lines
    (test--edit "" '((:op :delete-lines :start-line 1 :end-line 1)) nil)))


(fiveam:test newline-only-snapshots
  (dolist (ending (list (string #\Newline) (format nil "~C~C" #\Return #\Newline)))
    (fiveam:is (equalp (vector "") (test--split-lines ending)))
    (fiveam:signals invalid-operation
      (test--edit ending '((:op :replace-empty :content "x"))))
    (fiveam:is (string= "" (test--edit ending '((:op :delete-lines :start-line 1 :end-line 1)))))
    (fiveam:is (string= (concatenate 'string "x" ending)
                        (test--edit ending '((:op :replace-lines :start-line 1 :end-line 1 :content "x")))))
    (fiveam:is (string= (concatenate 'string ending "x" ending)
                        (test--edit ending '((:op :insert-after :line 1 :content "x")))))
    (fiveam:is (string= ending
                        (test--edit "" (list (list :op ':replace-empty :content ending)) nil)))))

(fiveam:test mixed-endings-rejected
  (let ((mixed (format nil "a~%b~C~C" #\Return #\Newline)))
    (fiveam:signals mixed-line-endings
      (test--edit mixed '((:op :delete-lines :start-line 1 :end-line 1))))
    (fiveam:is
     (string= (test--text "a" "b")
              (test--edit "a" (list (list :op ':replace-lines :start-line 1 :end-line 1
                                         :content mixed)))))
    (fiveam:signals mixed-line-endings
      (test--edit "" (list (list :op ':replace-empty :content mixed)) nil))))

(fiveam:test anchor-correction-and-ambiguity
  (let* ((text   (test--text "a" "b" "c" "d" "e" "f" "g"))
         (anchor (line-anchor "d")))
    (dolist (requested '(1 4 7))
      (multiple-value-bind (result normalized)
          (test--edit text (list (list :op ':replace-lines :start-line requested :end-line requested
                                      :start-anchor anchor :end-anchor anchor :content "x")))
        (fiveam:is (string= (test--text "a" "b" "c" "x" "e" "f" "g") result))
        (fiveam:is (= 4 (getf (first normalized) :start)))))
    (fiveam:signals anchor-error
      (test--edit text (list (list :op ':insert-before :line 1 :anchor (line-anchor "e") :content "x"))))
    (fiveam:signals anchor-error
      (edit-text text (list (list :op ':insert-before :line 1 :anchor anchor :content "x"))
                 :visible-lines (test--visible '(1 . 8)) :split-lines #'test--split-lines
                 :anchor-maximum-offset 2))
    (handler-case
        (progn
          (test--edit text (list (list :op ':insert-before :line 1 :anchor anchor :content "x"))
                      (test--visible '(1 . 2)))
          (fiveam:fail "Expected unmatched anchor for hidden target."))
      (anchor-error (condition)
        (fiveam:is (eq ':unmatched (anchor-error-reason condition))))))
  (dolist (requested '(1 2))
    (fiveam:signals anchor-error
      (test--edit (test--text "same" "other" "same")
                  (list (list :op ':insert-after :line requested :anchor (line-anchor "same") :content "x"))))))

(fiveam:test overlaps-rejected
  (dolist (operations '(((:op :delete-lines :start-line 1 :end-line 2)
                         (:op :replace-lines :start-line 2 :end-line 3 :content "x"))
                        ((:op :delete-lines :start-line 1 :end-line 2)
                         (:op :insert-before :line 2 :content "x"))
                        ((:op :insert-before :line 2 :content "x")
                         (:op :insert-before :line 2 :content "y"))
                        ((:op :insert-before :line 2 :content "x")
                         (:op :insert-after :line 2 :content "y"))))
    (fiveam:signals overlapping-operations
      (test--edit (test--text "a" "b" "c") operations))))

(fiveam:test publication-lifecycle
  (dolist (mode '(:success :stale :stage-failure :observe-failure :publish-failure :verify-failure :mismatch))
    (let ((events    nil)
          (base      (list :revision "exact-base"))
          (current   (list :revision "exact-current"))
          (published (list :revision "published"))
          (token     (list :temporary "staged")))
      (labels ((record (event) (push event events))

               (perform ()
                 (publish-edit
                  :base base :content "new"
                  :stage   (lambda (content)
                             (fiveam:is (string= "new" content))
                             (record ':stage)
                             (when (eq mode ':stage-failure) (error 'simple-error))
                             token)
                  :observe (lambda ()
                             (record ':observe)
                             (when (eq mode ':observe-failure) (error 'simple-error))
                             current)
                  :same-p  (lambda (actual expected)
                             (record ':same-p)
                             (fiveam:is (eq current actual))
                             (fiveam:is (eq base expected))
                             (not (eq mode ':stale)))
                  :publish (lambda (actual-token)
                             (record ':publish)
                             (fiveam:is (eq token actual-token))
                             (when (eq mode ':publish-failure) (error 'simple-error))
                             published)
                  :verify  (lambda (actual content)
                             (record ':verify)
                             (fiveam:is (eq published actual))
                             (fiveam:is (string= "new" content))
                             (when (eq mode ':verify-failure) (error 'simple-error))
                             (not (eq mode ':mismatch)))
                  :cleanup (lambda (actual-token)
                             (record ':cleanup)
                             (fiveam:is (eq token actual-token))))))
        (case mode
          (:success
           (fiveam:is (eq published (perform))))
          (:stale
           (handler-case (progn (perform) (fiveam:fail "Expected stale revision."))
             (stale-revision (condition)
               (fiveam:is (eq base (stale-revision-expected condition)))
               (fiveam:is (eq current (stale-revision-actual condition))))))
          (:mismatch
           (handler-case (progn (perform) (fiveam:fail "Expected publication mismatch."))
             (publication-error (condition)
               (fiveam:is (eq published (publication-error-actual condition))))))
          (otherwise
           (fiveam:signals error (perform))))
        (fiveam:is
         (equal (case mode
                  (:stage-failure
                   '(:stage))
                  (:observe-failure
                   '(:stage :observe :cleanup))
                  (:stale
                   '(:stage :observe :same-p :cleanup))
                  (:publish-failure
                   '(:stage :observe :same-p :publish :cleanup))
                  (otherwise
                   '(:stage :observe :same-p :publish :verify :cleanup)))
                (reverse events)))))))

(defun run-tests ()
  "Run the complete hashline suite and return a boolean success value."
  (not (null (fiveam:run! 'hashline-tests))))
