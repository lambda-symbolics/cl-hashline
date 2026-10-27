(asdf:defsystem #:cl-hashline
  :description "Line-anchored, revision-gated text edits."
  :author      "Lukáš Hozda"
  :license     "COLL-Attribution"
  :version     "0.1.0"
  :serial      t
  :depends-on  (#:babel #:ironclad #:structlisp)
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-hashline/tests)))
  :components  ((:file "src/package")
                (:file "src/conditions")
                (:file "src/text")
                (:file "src/edit")
                (:file "src/publish")))

(asdf:defsystem #:cl-hashline/tests
  :description "Behavioral tests for cl-hashline."
  :depends-on  (#:cl-hashline #:fiveam)
  :serial      t
  :components  ((:file "tests/tests"))
  :perform     (asdf:test-op (operation component)
                 (declare (ignore operation component))
                 (unless (uiop:symbol-call '#:cl-hashline '#:run-tests)
                   (error "cl-hashline tests failed."))))
