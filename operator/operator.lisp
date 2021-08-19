(in-package :fusion-category.operator)

(defmacro def-multiary-operator (op-symbol)
  "Expect OP-SYMBOL-BINARY to be defined as a commutative binary
operator. Return its right associative multi-ary version of
operator."
  `(defun ,op-symbol (&rest xs)
     (case (length xs)
       (0 nil)
       (1 (car xs))
       (2 (apply (function ,(read-from-string (format nil "~a-binary" op-symbol))) xs))
       (t (,op-symbol (car xs) (,op-symbol (cdr xs)))))))
