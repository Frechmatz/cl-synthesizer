(in-package :cl-synthesizer-test)

(defun test-csv-writer-read-output (str)
  (let ((the-stream (make-string-input-stream str))
	(read-strings nil))
    (loop
	 (let ((s (read-line the-stream nil nil)))
	       (if (not s)
		   (return)
		   (push s read-strings))))
    (reverse read-strings)))

(define-test test-csv-file-writer-1 ()
	     (let ((open-stream-called nil)
		   (close-stream-called nil)
		   (output-stream nil)
		   (written-string nil))
	       (let ((cl-synthesizer-modules-csv-file-writer::*make-writer*
		      (lambda (&rest args)
			(declare (ignore args))
			(list
			 :open-stream (lambda (&rest args)
					(declare (ignore args))
					(setf open-stream-called t)
					(setf output-stream (make-string-output-stream))
					output-stream)
			 :close-stream (lambda ()
					 (setf close-stream-called t)
					 (setf written-string (get-output-stream-string output-stream))
					 (close output-stream))))))
		 (let ((module (cl-synthesizer-modules-csv-file-writer:make-module
				"CSV-File-Writer"
				(cl-synthesizer:make-environment)
				:columns '((:name "Col-1" :default-value "Default Col-1") (:name "Col-2" :default-value "Default Col-2"))
				:filename "test"
				:column-separator ","
				:add-header t)))
		   (update-module module (list (list :column-1 "1.1") (list :column-2 "1.2")))
		   (update-module module (list (list :column-1 "2.1") (list :column-2 "2.2")))
		   (update-module module (list (list :column-1 nil) (list :column-2 "3.2")))
		   (update-module module (list (list :column-1 "4.1") (list  :column-2 nil)))
		   (cl-synthesizer:shutdown module)
		   (assert-true (< 0 (length written-string)))
		   (assert-true open-stream-called)
		   (assert-true close-stream-called)
		   (let ((lines (test-csv-writer-read-output written-string)))
		     (format t "~%Lines: ~a~%" lines)
		     (assert-true (= 5 (length lines)))
		     (assert-equal "Col-1,Col-2" (first lines))
		     (assert-equal "\"1.1\",\"1.2\"" (second lines))
		     (assert-equal "\"2.1\",\"2.2\"" (third lines))
		     (assert-equal "\"Default Col-1\",\"3.2\"" (fourth lines))
		     (assert-equal "\"4.1\",\"Default Col-2\"" (fifth lines)))))))

