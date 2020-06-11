;;;; dl-list.asd

(asdf:defsystem #:dl-list
  :description "Decent double-linked list implementation"
  :author "commander-thrashdin"
  :license  "CCO"
  :version "0.1"
  :serial t
  :components ((:file "package")
               (:file "dl-list"
                :around-compile (lambda (thunk)
                                  (apply thunk (when (uiop:featurep :sbcl)
                                                 '(:block-compile t)))))))
