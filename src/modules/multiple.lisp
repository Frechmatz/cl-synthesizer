(in-package :cl-synthesizer-modules-multiple)

(defmacro multiple (name output-count)
  (let ((output-name "out"))
    `(defun ,(cl-synthesizer-macro-util::make-package-symbol name nil) (name environment)
       (declare (ignore environment name))
       (let ((cur-input nil)
	     (inputs (list :input))
	     (outputs (cl-synthesizer-macro-util::make-keyword-list ,output-name ,output-count)))
	 (list
	  :inputs (lambda () inputs)
	  :outputs (lambda () outputs)
	  :get-output (lambda (output)
			(declare (ignore output))
			cur-input)
	  :update (lambda (&key input)
		    (setf cur-input input))
	  )))))

(multiple "multiple-4" 4)


