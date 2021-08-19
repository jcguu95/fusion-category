(in-package :fusion-category.ordered-set)

;; TODO Move the utils for arrays, hash-tables, and lists to
;; another minion file.
;;
;; TODO More docstrings.

(defclass ordered-set ()
  ;; An ordered-set is a vector with a hash table that maps keys
  ;; to positions.
  ;;
  ;; TODO sanity check: keys is an array of non-repeating keys,
  ;; there must be a bijection between {1.. size}, keys, and the
  ;; images of keys.
  ;;
  ;; Use the constructors #'ordered-set<-list, product-binary, coproduct-binary
  ;; to safely construct an ordered set.
  ;;
  ;; Use #'element to lookup element in an ordered set. TODO It's
  ;; not setf-able yet.
  ((content
    :initform #()
    :initarg :content
    :accessor content)
   (lookup-table
    :initform (make-hash-table)
    :initarg :lkup-table
    :accessor lkup-table)))

(defmethod initialize-instance :after ((os ordered-set) &key)
  (assert (not (has-duplicate-element? (content os)))))

(defgeneric has-duplicate-elements (collection))
(defmethod has-duplicate-element? ((v vector))
  (> (length v) (length (remove-duplicates v :test #'equalp))))

(defgeneric size (collection))
(defmethod size ((os ordered-set))
  (length (content os)))

(defmethod keys ((h hash-table))
  ;; ht utils
  (loop for key being the hash-keys of h collect key))

(defmethod ordered-set<-list ((xs list))
  "A constructor of an ordered set from a list."
  (let ((set (make-instance 'ordered-set))
        (length (length xs)))
    (setf (content set) (coerce xs 'vector))
    (loop for i from 0 to (- length 1)
          do (setf (gethash i (lkup-table set)) i))
    set))

(defun generate-ordered-set (size)
  (assert (integerp size))
  (assert (>= size 0))
  (ordered-set<-list (loop for i from 1 to size collect (gensym))))

(defgeneric element (query collection))

;; TODO Make #'element setf-able.
(defmethod element (query (os ordered-set))
  "Expect QUERY to be a number or a list of a single key."
  (if (atom query)
      (elt (content os) query)
      (elt (content os) (gethash (car query) (lkup-table os)))))

(defgeneric coproduct-binary (x y))

(defmethod coproduct-binary ((h0 hash-table) (h1 hash-table))
  ;; ht util
  "Return the union of the hash tables."
  (let ((h (make-hash-table)))
    (maphash
     (lambda (k v) (setf (gethash (list 0 k) h) v))
     (alexandria:copy-hash-table h0))
    (maphash
     (lambda (k v) (setf (gethash (list 1 k) h) v))
     (alexandria:copy-hash-table h1))
    h))

(defmethod coproduct-binary ((x0 list) (x1 list))
  ;; list util
  (copy-seq (concatenate 'list x0 x1)))

(defmethod coproduct-binary ((v0 array) (v1 array))
  ;; array util
  (let ((l0 (length v0))
        (l1 (length v1)))
    (make-array (+ l0 l1)
                :initial-contents
                (coproduct-binary (coerce v0 'list) (coerce v1 'list)))))

(defmethod coproduct-binary ((s0 ordered-set) (s1 ordered-set))
  "Return the union ordered set of s0 and s1."
  (let* ((c0 (copy-seq (content s0)))
         (c1 (copy-seq (content s1)))
         (lt0 (alexandria:copy-hash-table (lkup-table s0)))
         (lt1 (alexandria:copy-hash-table (lkup-table s1)))
         (shift (length c0)))
    (maphash #'(lambda (k v) (setf (gethash k lt1) (+ v shift))) lt1)
    (make-instance 'ordered-set
                   :content (coproduct-binary c0 c1)
                   :lkup-table (coproduct-binary lt0 lt1))))

(defgeneric product-binary (collection-0 collection-1))
(defmethod product-binary ((x0 list) (x1 list))
  ;; list util
  (loop for x in x0
        collect (loop for x- in x1
                      collect (list x x-))))

(defmethod product-binary ((v0 vector) (v1 vector))
  ;; array util
  ;; Usually used with #'aops:flatten to get back to a 1D array.
  (make-array (list (length v0) (length v1))
              :initial-contents
              (product-binary (coerce v0 'list) (coerce v1 'list))))

(defmethod product-binary ((h0 hash-table) (h1 hash-table))
  ;; ht util
  "Return the product-binary of the hash tables."
  (let ((h (make-hash-table))
        (h0 (alexandria:copy-hash-table h0))
        (h1 (alexandria:copy-hash-table h1)))
    (loop for key0 in (keys h0)
          do (loop for key1 in (keys h1)
                   do (setf (gethash (list key0 key1) h)
                            (list (gethash key0 h0)
                                  (gethash key1 h1)))))
    h))

(defmethod product-binary ((os0 ordered-set) (os1 ordered-set))
  (let ((lt0 (alexandria:copy-hash-table (lkup-table os0)))
        (lt1 (alexandria:copy-hash-table (lkup-table os1)))
        (c0 (copy-seq (content os0)))
        (c1 (copy-seq (content os1))))
    (make-instance 'ordered-set
                   :content (aops:flatten (product-binary c0 c1))
                   :lkup-table (product-binary lt0 lt1))))

;; TODO Maybe I should call product <*> and coproduct <+> as in before.

(fusion-category.operator:def-multiary-operator product)
(fusion-category.operator:def-multiary-operator coproduct)
