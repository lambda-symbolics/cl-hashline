(defpackage #:cl-hashline
  (:documentation "Line-anchored text edits checked against an observed snapshot and its revision.")
  (:use #:cl)
  (:export #:line-anchor
           #:line-ending
           #:mixed-line-endings-p
           #:final-newline-p
           #:join-lines
           #:edit-text
           #:publish-edit
           #:hashline-error
           #:invalid-operation
           #:invalid-operation-operation
           #:invalid-operation-field
           #:invalid-operation-value
           #:invalid-operation-reason
           #:unseen-lines
           #:unseen-lines-start-line
           #:unseen-lines-end-line
           #:anchor-error
           #:anchor-error-anchor
           #:anchor-error-requested-line
           #:anchor-error-field
           #:anchor-error-matches
           #:anchor-error-reason
           #:overlapping-operations
           #:overlapping-operations-left
           #:overlapping-operations-right
           #:mixed-line-endings
           #:stale-revision
           #:stale-revision-expected
           #:stale-revision-actual
           #:publication-error
           #:publication-error-actual))
