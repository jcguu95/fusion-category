(in-package :fusion-category)

(defmacro def-multiary-operator (op)
  "Expect OP-SYMBOL-BINARY to be defined as a commutative binary
operator. Return its right associative multi-ary version of
operator."
  `(defun ,op (&rest xs)
     (let ((op2 (read-from-string (format nil "~a-binary" ',op))))
       (case (length xs)
         (0 nil)
         (1 (car xs))
         (2 (apply op2 xs))
         (t (,op (car xs) (apply ',op (cdr xs))))))))
