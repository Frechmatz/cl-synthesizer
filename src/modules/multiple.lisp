(in-package :cl-synthesizer-modules-multiple)

(defmacro multiple (name output-count)
  "A multiple with n outputs and one input 'input'"
  (let ((output-name "out"))
    `(defun ,(cl-synthesizer-macro-util::make-package-symbol name nil) (name environment)
       (declare (ignore environment name))
       (let ((cur-input nil))
	 (list
	  :inputs (lambda () '(:input))
	  :outputs (lambda () (cl-synthesizer-macro-util::make-keyword-list ,output-name ,output-count))
	  :get-output (lambda (output)
			(declare (ignore output))
			cur-input)
	  :update (lambda (&key input)
		    (setf cur-input input))
	  )))))

(multiple "multiple-4" 4)


