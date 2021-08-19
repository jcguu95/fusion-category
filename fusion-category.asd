#-asdf3.1 (error "requires ASDF 3.1")

(asdf:defsystem "fusion-category"
  :description "A calculator for fusion categories."
  :author "Jin <jcguu95@gmail.com>"
  :licence "GPLv3"
  :version "0.0"
  #+asdf-unicode :encoding #+asdf-unicode :utf-8
  :components ()
  :depends-on (:alexandria :array-operations :closer-mop
               "fusion-category.operator"
               "fusion-category.ordered-set"
               "fusion-category.vector-space"))
