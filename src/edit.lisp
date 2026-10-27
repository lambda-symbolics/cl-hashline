(in-package #:cl-hashline)

;;;; -- Operation Validation --

(defun edit--proper-list-p (value)
  "Return true for a finite proper list, including the empty list."
  (and (listp value)
       (handler-case (not (null (list-length value)))
         (type-error () nil))))

(defun edit--invalid (operation reason &key field value)
  "Signal a structured validation failure for OPERATION."
  (error 'invalid-operation :operation operation :reason reason
                           :field field :value value))

(defun edit--validate-fields (operation allowed)
  "Reject malformed plists, repeated keys, and keys outside ALLOWED."
  (unless (and (edit--proper-list-p operation) (evenp (length operation)))
    (edit--invalid operation ':malformed-plist))
  (let ((seen nil))
    (loop for (key value) on operation by (lambda (tail) (nthcdr 2 tail))
          do (unless (member key allowed)
               (edit--invalid operation ':unsupported-field :field key :value value))
             (when (member key seen)
               (edit--invalid operation ':duplicate-field :field key :value value))
             (push key seen))))

(defun edit--field (operation field &key predicate reason)
  "Read a required FIELD satisfying PREDICATE or report REASON."
  (let* ((missing (gensym "MISSING"))
         (value   (getf operation field missing)))
    (when (eq value missing)
      (edit--invalid operation ':missing-field :field field))
    (unless (funcall predicate value)
      (edit--invalid operation reason :field field :value value))
    value))

(defun edit--positive-line (operation field)
  "Read a required positive original line number."
  (edit--field operation field
               :predicate (lambda (value) (and (integerp value) (plusp value)))
               :reason ':invalid-line))

(defun edit--content (operation &key non-empty-p)
  "Read replacement or insertion content from OPERATION."
  (edit--field operation ':content
               :predicate (lambda (value)
                            (and (stringp value)
                                 (or (not non-empty-p) (plusp (length value)))))
               :reason ':invalid-content))

(defun edit--anchor (operation field)
  "Read an optional four-hex FIELD and normalize it to uppercase."
  (let ((anchor (getf operation field)))
    (when anchor
      (unless (and (stringp anchor) (= (length anchor) 4)
                   (every (lambda (character) (digit-char-p character 16)) anchor))
        (edit--invalid operation ':invalid-anchor :field field :value anchor))
      (string-upcase anchor))))

(defun edit--split-lines (content split-lines)
  "Invoke the caller's logical line splitter and validate its result shape."
  (let ((lines (funcall split-lines content)))
    (unless (and (vectorp lines) (not (stringp lines)) (every #'stringp lines))
      (edit--invalid nil ':invalid-split-result :value lines))
    lines))

(defun edit--visible-lines (visible-lines total)
  "Validate the observed interval set against TOTAL original lines."
  (let ((visible (or visible-lines (structlisp:make-integer-interval-set))))
    (unless (typep visible 'structlisp:integer-interval-set)
      (edit--invalid nil ':invalid-visible-lines :value visible))
    (loop for interval across (structlisp:integer-interval-set->vector visible)
          unless (<= 1 (structlisp:integer-interval-start interval)
                     (structlisp:integer-interval-end interval) (1+ total))
            do (edit--invalid nil ':invalid-visible-lines :value visible))
    visible))

(defun edit--resolve-line (lines visible requested-line &key anchor field maximum-offset)
  "Resolve an optional anchor uniquely within the bounded visible correction window."
  (unless anchor
    (return-from edit--resolve-line requested-line))
  (let* ((matches
           (loop for interval across (structlisp:integer-interval-set->vector visible)
                 append (loop for line from (structlisp:integer-interval-start interval)
                              below (structlisp:integer-interval-end interval)
                              when (string= anchor (line-anchor (aref lines (1- line))))
                                collect line)))
         (nearby (remove-if (lambda (line)
                              (> (abs (- line requested-line)) maximum-offset))
                            matches))
         (reason (cond
                   ((null matches)
                    ':unmatched)
                   ((null nearby)
                    ':too-far)
                   ((rest nearby)
                    ':ambiguous))))
    (when reason
      (error 'anchor-error :anchor anchor :requested-line requested-line
                           :field field :matches matches :reason reason))
    (first nearby)))

(defun edit--normalize-operation (operation lines &key visible split-lines maximum-offset)
  "Validate one keyword plist against exact original LINES and VISIBLE intervals."
  (unless (and (edit--proper-list-p operation) (evenp (length operation)))
    (edit--invalid operation ':malformed-plist))
  (let ((name (getf operation :op)))
    (unless (member name '(:replace-lines :delete-lines :insert-before
                          :insert-after :replace-empty))
      (edit--invalid operation ':unknown-operation :field ':op :value name))
    (case name
      (:replace-empty
       (edit--validate-fields operation '(:op :content))
       (unless (zerop (length lines))
         (edit--invalid operation ':nonempty-base))
       (let ((content (edit--content operation :non-empty-p t)))
         (when (mixed-line-endings-p content)
           (error 'mixed-line-endings))
         (list :kind ':replace-empty :start 0 :end 0
               :lines (coerce (edit--split-lines content split-lines) 'list)
               :line-ending (line-ending content)
               :final-newline-p (final-newline-p content)
               :summary "replace-empty")))
      ((:replace-lines :delete-lines)
       (let ((replace-p (eq name ':replace-lines)))
         (edit--validate-fields
          operation (if replace-p
                        '(:op :start-line :end-line :start-anchor :end-anchor :content)
                        '(:op :start-line :end-line :start-anchor :end-anchor)))
         (let ((start (edit--resolve-line
                       lines visible (edit--positive-line operation ':start-line)
                       :anchor (edit--anchor operation ':start-anchor)
                       :field ':start-line :maximum-offset maximum-offset))
               (end   (edit--resolve-line
                       lines visible (edit--positive-line operation ':end-line)
                       :anchor (edit--anchor operation ':end-anchor)
                       :field ':end-line :maximum-offset maximum-offset)))
           (unless (<= start end)
             (edit--invalid operation ':reversed-range))
           (unless (structlisp:integer-interval-set-covers-p visible start (1+ end))
             (error 'unseen-lines :start-line start :end-line end))
           (list :kind (if replace-p ':replace ':delete) :start start :end end
                 :lines (when replace-p
                          (coerce (edit--split-lines (edit--content operation) split-lines)
                                  'list))
                 :summary (format nil "~(~A~) ~D-~D" name start end)))))
      ((:insert-before :insert-after)
       (edit--validate-fields operation '(:op :line :anchor :content))
       (let ((line (edit--resolve-line
                    lines visible (edit--positive-line operation ':line)
                    :anchor (edit--anchor operation ':anchor)
                    :field ':line :maximum-offset maximum-offset)))
         (unless (structlisp:integer-interval-set-contains-p visible line)
           (error 'unseen-lines :start-line line :end-line line))
         (list :kind name :start line :end line
               :lines (coerce (edit--split-lines
                               (edit--content operation :non-empty-p t) split-lines)
                              'list)
               :summary (format nil "~(~A~) ~D" name line)))))))

(defun edit--normalize-operations (operations lines &key visible split-lines maximum-offset)
  "Validate and sort non-overlapping operations in original-line order."
  (unless (and operations (edit--proper-list-p operations))
    (edit--invalid operations ':empty-or-malformed-operations))
  (let ((normalized
          (loop for operation in operations
                collect (edit--normalize-operation
                         operation lines :visible visible :split-lines split-lines
                                         :maximum-offset maximum-offset))))
    (when (and (find ':replace-empty normalized :key (lambda (op) (getf op :kind)))
               (rest normalized))
      (edit--invalid operations ':combined-replace-empty))
    (setf normalized (sort normalized #'< :key (lambda (op) (getf op :start))))
    (loop for (left right) on normalized
          while right
          when (<= (getf right :start) (getf left :end))
            do (error 'overlapping-operations :left left :right right))
    normalized))


;;;; -- Single-Pass Editing --

(defun edit--apply-operations (lines operations)
  "Apply sorted normalized OPERATIONS to original LINES in one pass."
  (when (eq (getf (first operations) :kind) ':replace-empty)
    (return-from edit--apply-operations (copy-list (getf (first operations) :lines))))
  (let ((result    nil)
        (line      1)
        (remaining operations)
        (total     (length lines)))
    (loop while (<= line total)
          for operation = (first remaining)
          do (cond
               ((and operation (= line (getf operation :start)))
                (case (getf operation :kind)
                  (:insert-before
                   (dolist (new-line (getf operation :lines)) (push new-line result))
                   (push (aref lines (1- line)) result)
                   (incf line))
                  (:insert-after
                   (push (aref lines (1- line)) result)
                   (dolist (new-line (getf operation :lines)) (push new-line result))
                   (incf line))
                  ((:replace :delete)
                   (dolist (new-line (getf operation :lines)) (push new-line result))
                   (setf line (1+ (getf operation :end)))))
                (pop remaining))
               (t
                (push (aref lines (1- line)) result)
                (incf line))))
    (nreverse result)))

(defun edit-text (content operations &key visible-lines split-lines (anchor-maximum-offset 3))
  "Apply keyword-plist OPERATIONS to exact CONTENT and return text and normalized operations.

VISIBLE-LINES is a STRUCTLISP integer interval set of fully observed one-based
lines, using half-open bounds. Omitted visibility grants no lines. SPLIT-LINES
is required and receives a string, returning a vector of logical strings with
LF/CRLF endings removed and no phantom line after a terminal newline. Empty
content must split to an empty vector.

Anchors identify the first four SHA-256 hex digits of UTF-8 logical lines. A
supplied anchor must match exactly one visible line within ANCHOR-MAXIMUM-OFFSET
of the requested number, even when that number already matches. All operation
coordinates refer to the original content. Inputs and visibility are not mutated."
  (check-type content string)
  (unless (functionp split-lines)
    (edit--invalid operations ':missing-splitter))
  (unless (and (integerp anchor-maximum-offset) (not (minusp anchor-maximum-offset)))
    (edit--invalid operations ':invalid-maximum-offset :value anchor-maximum-offset))
  (when (mixed-line-endings-p content)
    (error 'mixed-line-endings))
  (let* ((lines      (edit--split-lines content split-lines))
         (visible    (edit--visible-lines visible-lines (length lines)))
         (normalized (edit--normalize-operations
                      operations lines :visible visible :split-lines split-lines
                                       :maximum-offset anchor-maximum-offset))
         (empty-op   (find ':replace-empty normalized :key (lambda (op) (getf op :kind)))))
    (values (join-lines (edit--apply-operations lines normalized)
                       (if empty-op (getf empty-op :line-ending) (line-ending content))
                       (if empty-op (getf empty-op :final-newline-p) (final-newline-p content)))
            normalized)))
