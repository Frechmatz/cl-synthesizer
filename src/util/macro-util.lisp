(in-package :cl-synthesizer-macro-util)

(defun get-socket-number (i)
  (+ i 1))

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) (get-socket-number num)) package)
      (intern (string-upcase name) package)))

(defun make-package-symbol (name num)
  (make-symbol-impl name num *PACKAGE*))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))

(defun make-let-list (name count)
  (let ((l nil))
    (dotimes (i count)
      (push (list (make-package-symbol name i) nil) l))
    l))

(defun make-param-list (name count)
  (let ((l nil))
    (dotimes (i count)
      (push (make-package-symbol name i) l))
    (nreverse l)))

(defun make-keyword-list (name count)
  "Returns list of keywords ordered by number of keyword: (:<name>-1, :<name>-2, ..., <name>-<count>.
   The numbering starts by one."
  (let ((l nil))
    (dotimes (i count)
      (push (make-keyword name i) l))
    (nreverse l)))
