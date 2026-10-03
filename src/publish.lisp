(in-package #:cl-hashline)

;;;; -- Revision-Gated Publication --

(defun publish-edit (&key base content stage observe same-p publish cleanup verify)
  "Stage CONTENT, check the exact BASE, publish atomically, and verify the result.

STAGE receives CONTENT and returns a token for prepared storage. OBSERVE returns
current storage state; SAME-P receives that state and BASE. PUBLISH receives the
token and returns the published state. VERIFY receives that state and CONTENT.
CLEANUP receives the token on every exit after staging, including success.
STAGE must clean up its own partial work if it fails before returning a token.

The caller owns storage, exact snapshot equality, serialization, and atomic
publication. Existing-file replacement may have a check-to-publish race with
external writers unless the storage adapter supports conditional publication."
  (let ((token (funcall stage content)))
    (unwind-protect
         (progn
           (let ((current (funcall observe)))
             (unless (funcall same-p current base)
               (error 'stale-revision :expected base :actual current)))
           (let ((published (funcall publish token)))
             (unless (funcall verify published content)
               (error 'publication-error :actual published))
             published))
      (funcall cleanup token))))
