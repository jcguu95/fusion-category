(in-package fusion-category.vector-space)

(defclass vector-space ()
  ((dimension
    :initarg :dim
    :accessor dim)
   (basis
    :initarg :basis
    :accessor basis)))

(defmethod initialize-instance :after ((V vector-space) &key)
  (assert (integerp (dim V)))
  (assert (>= (dim V) 0))
  (assert (eq (class-of (basis V)) (find-class 'fusion-category.ordered-set::ordered-set)))
  (assert (= (fusion-category.ordered-set::size (basis V)) (dim V))))

(defun vector-space<-ordered-set (os)
  (let ((dim (fusion-category.ordered-set::size os)))
    (make-instance 'vector-space
                   :dim dim
                   :basis os)))

;; Definition of #'vector-space<-list
(setf (fdefinition 'vector-space<-list)
      (lambda (xs)
        (vector-space<-ordered-set
         (fusion-category.ordered-set::ordered-set<-list xs))))
;; Definition of #'vs<-list
(setf (fdefinition 'vs<-list) #'vector-space<-list)

(defclass vect ()
  ((ambient-space
    :initarg :amb
    :accessor amb)
   (coefficients
    :initarg :coef
    :accessor coef)))

(defmethod initialize-instance :after ((v vect) &key)
  (let* ((amb (amb v))
         (dim (dim amb))
         (coef (coef v))
         (size (length coef)))
    ;; FIXME I only what the class of AMB be a subclass of 'VECTOR-SPACE.
    (assert (c2mop:subclassp (class-of amb) (find-class 'vector-space)))
    (assert (= dim size))
    (loop for c in coef
          do (assert (numberp c)))))

(defmethod make-vect ((V vector-space) (coeffs list))
  (make-instance 'vect :amb V :coef coeffs))

(defun crandom (&optional (n 3))
  "Generate a not so random complex number."
  (+ (random n) (* (random n) #c(0 1))))

(defmethod example ((V vector-space))
  "Generate a random vector for the vector space V."
  (let ((dim (dim V)))
    (make-vect V (loop for i from 1 to dim collect (crandom 3)))))

(defmethod <+>-binary ((v0 vect) (v1 vect))
  "Sum of two vectors."
  (let ((c0 (coef v0)) (A0 (amb v0))
        (c1 (coef v1)) (A1 (amb v1)))
    (assert (equal A0 A1))
    (make-vect A0
               (loop for i in c0
                     for j in c1
                     collect (+ i j)))))

(defmethod <*>-binary ((s number) (v vect))
  "Scalar product of a vector."
  (let ((c (coef v)) (A (amb v)))
    (make-vect A (loop for i in c
                       collect (* s i)))))

(defmethod <*>-binary ((v vect) (s number))
  "Scalar product of a vector."
  (<*>-binary s v))

(defmethod <+>-binary ((V0 vector-space)
                (V1 vector-space))
  "Direct sum of two vector spaces."
  (let ((d0 (dim V0)) (b0 (basis V0))
        (d1 (dim V1)) (b1 (basis V1)))
    (make-instance 'vector-space
                   :dim (+ d0 d1)
                   :basis (fusion-category.ordered-set::coproduct b0 b1))))

(defmethod <*>-binary ((V0 vector-space)
                (V1 vector-space))
  "Tensor product of two vector spaces."
  (let ((d0 (dim V0)) (b0 (basis V0))
        (d1 (dim V1)) (b1 (basis V1)))
    (make-instance 'vector-space
                   :dim (* d0 d1)
                   :basis (fusion-category.ordered-set::product b0 b1))))

(defmethod coproduct-binary ((V0 vector-space)
                             (V1 vector-space))
  "Direct sum of two vector spaces."
  (<+>-binary V0 V1))

(defmethod product-binary ((V0 vector-space)
                           (V1 vector-space))
  "Tensor product of two vector spaces."
  (<*>-binary V0 V1))

(defclass dual-vector-space (vector-space)
  ((pre-dual
    :initarg :pre-dual
    :accessor pre-dual)))

(defmethod dual ((V vector-space))
  (make-instance 'dual-vector-space
                 :dim (dim V)
                 :basis (basis V)
                 :pre-dual V))

(defmethod contract ((v0 vect) (v1 vect))
  "Check if v0 and v1 come from a pair of dual vector spaces. If
so, take the naive inner product of their coefficients."
  (let ((a0 (amb v0))
        (a1 (amb v1)))
    (cond ((or (and (equal (class-of a0) (find-class 'dual-vector-space))
                    (equal (pre-dual a0) a1))
               (and (equal (class-of a1) (find-class 'dual-vector-space))
                    (equal (pre-dual a1) a0)))
           (loop for c0 in (coef v0)
                 for c1 in (coef v1)
                 summing (* c0 c1) into total
                 finally (return total)))
          (t (error "One of the vector must be in the dual of the
        ambient space of the other vector.")))))

(fusion-category.operator:def-multiary-operator <+>)
(fusion-category.operator:def-multiary-operator <*>)
