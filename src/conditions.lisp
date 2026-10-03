(in-package #:cl-hashline)

;;;; -- Edit Conditions --

(define-condition hashline-error (error)
  ()
  (:documentation "Base condition for invalid or unpublishable text edits."))

(define-condition invalid-operation (hashline-error)
  ((operation :initarg :operation :reader invalid-operation-operation
              :documentation "The rejected input operation or operation list.")
   (field     :initarg :field :initform nil :reader invalid-operation-field
              :documentation "The invalid keyword field, when applicable.")
   (value     :initarg :value :initform nil :reader invalid-operation-value
              :documentation "The invalid field value, when applicable.")
   (reason    :initarg :reason :reader invalid-operation-reason
              :documentation "A keyword describing the validation failure."))
  (:documentation "An edit operation violates the normalized plist contract.")
  (:report (lambda (condition stream)
             (format stream "Invalid edit operation~@[ field ~S~]: ~A."
                     (invalid-operation-field condition)
                     (invalid-operation-reason condition)))))

(define-condition unseen-lines (hashline-error)
  ((start-line :initarg :start-line :reader unseen-lines-start-line
               :documentation "The first targeted original line.")
   (end-line   :initarg :end-line :reader unseen-lines-end-line
               :documentation "The last targeted original line."))
  (:documentation "An edit targets original lines outside the observed ranges.")
  (:report (lambda (condition stream)
             (format stream "Lines ~D-~D were not fully visible under this revision. Reread the required window."
                     (unseen-lines-start-line condition)
                     (unseen-lines-end-line condition)))))

(define-condition anchor-error (hashline-error)
  ((anchor         :initarg :anchor :reader anchor-error-anchor
                   :documentation "The normalized four-hex line anchor.")
   (requested-line :initarg :requested-line :reader anchor-error-requested-line
                   :documentation "The caller's original line number.")
   (field          :initarg :field :reader anchor-error-field
                   :documentation "The operation field carrying that number.")
   (matches        :initarg :matches :reader anchor-error-matches
                   :documentation "Matching visible original line numbers.")
   (reason         :initarg :reason :reader anchor-error-reason
                   :documentation "One of :unmatched, :too-far, or :ambiguous."))
  (:documentation "A line anchor cannot uniquely correct a visible target.")
  (:report (lambda (condition stream)
             (format stream "Anchor ~A for ~A at line ~D is ~A; matching visible lines: ~S. Reread the required window."
                     (anchor-error-anchor condition)
                     (anchor-error-field condition)
                     (anchor-error-requested-line condition)
                     (anchor-error-reason condition)
                     (anchor-error-matches condition)))))

(define-condition overlapping-operations (hashline-error)
  ((left  :initarg :left :reader overlapping-operations-left
          :documentation "The first normalized operation.")
   (right :initarg :right :reader overlapping-operations-right
          :documentation "The conflicting normalized operation."))
  (:documentation "Two operations overlap or share an original line anchor.")
  (:report (lambda (condition stream)
             (let ((left  (overlapping-operations-left condition))
                   (right (overlapping-operations-right condition)))
               (format stream "Edit operations overlap or share original lines ~D-~D and ~D-~D."
                       (getf left :start) (getf left :end)
                       (getf right :start) (getf right :end))))))

(define-condition mixed-line-endings (hashline-error)
  ()
  (:documentation "The exact input cannot be reconstructed with one line ending.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Structured edits cannot preserve mixed LF and CRLF line endings. Normalize the complete content deliberately." stream))))

(define-condition stale-revision (hashline-error)
  ((expected :initarg :expected :reader stale-revision-expected
             :documentation "The caller's exact base snapshot.")
   (actual   :initarg :actual :reader stale-revision-actual
             :documentation "The snapshot found immediately before publication."))
  (:documentation "Storage no longer matches the exact base snapshot.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The base revision is stale. Observe storage again before editing." stream))))

(define-condition publication-error (hashline-error)
  ((actual :initarg :actual :reader publication-error-actual
           :documentation "The unexpected state returned by the publication adapter."))
  (:documentation "Publication did not produce the exact requested content.")
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Atomic publication did not produce the exact requested content." stream))))
