#-asdf3.1 (error "requires ASDF 3.1")

(asdf:defsystem :fusion-category
  :serial t
  :depends-on (:alexandria
               :array-operations
               :closer-mop)
  :components ((:module "src"
                :components ((:file "package")
                             (:file "operator")
                             (:file "ordered-set")
                             (:file "vector-space")
                             (:file "fusion-category")
                             ))))
