(in-package :fusion-category)

(defclass multifusion-category ()
  ((rank
    :initarg :rank
    :accessor rank)
   (basis
    :initarg :basis
    :accessor basis)
   (id-object-formula
    :initarg :id-object-formula
    :accessor iof)
   (fusion-rule
    :initarg :fusion-rule
    :accessor fusion-rule)
   (associator
    :initarg :associator
    :accessor associator)
   ;; (l-and-r) ?
   ))

(defclass fusion-category (multifusion-category)
  ())

(defclass object ()
  ((ambient-category
    :initarg :ambient-category
    :accessor amb)
   (data
    ;; Notice that (ij)k is NOT i(jk), so we need to encode the
    ;; data as a binary tree, whose leaves contain a list of
    ;; nonnegative integers with length equal to rank of C.
    ;; Things like (((1 3 2)(1 0 1))(2 0 0)) if rank C is 3.
    ;; TODO Look for a binary tree package?
    :initarg :data
    :accessor data))
  )

(defmethod initialize-instance :after ((C multifusion-category) &key)
  ;; TODO Make a copy of each slot.. we want immutability.
  (assert (integerp (rank C)))
  (assert (>= (rank C) 0))
  (assert (eq (class-of (basis C))
              (find-class 'ordered-set)))
  (assert (= (size (basis C)) (rank C)))
  (assert (every (lambda (x)
                   (and (integerp x) (>= x 0)))
                 (iof C)))
  ;; (assert (fusion-data? (fusion-rule C))) ;; TODO not yet implemented
  ;;
  ;; e.g. associativity, respecting identity.. etc
  ;;
  ;; --
  ;;
  ;; (assert (associator-data? (associator C))) ;; TODO not yet implemented
  ;;
  ;; For each i,j,k,l, we need an alpha (Mor i (jk)l) to (Mor i
  ;; j(kl)) satisfying pentagon axiom. Note - We don't have
  ;; symbolic algebraic numbers, so we can check only up to some
  ;; given epsilon > 0.
  )

(defgeneric isomorphism-type (X)
  ;; X: object
  ;; "Compute the isomorphism type as a direct sum in terms of the
  ;; chosen simple objects."
  )

(defgeneric gen-object (C))
(defgeneric nth-simple (n C))

(defgeneric Mor (X Y)
  ;; "Return the morphism space from X to Y."
  ;; (assert (equal (amb X) (amb Y)))
  ;; TODO
  ;;
  ;; 1. If X is simple, the implementation will use induction on
  ;; the depth of Y.
  ;;
  ;; 2. if Y is simple, return (dual (Mor Y X)).
  ;;
  ;; 3. Either.. then return
  ;;
  ;;    (apply #'<+>
  ;;       (loop for p in (basis C)
  ;;             collect (<*> (Mor X p) (Mor p Y))))
  ;;
  )
