(in-package :cl-synthesizer-modules-multiple)

(defun multiple (name environment &key output-count)
  "A multiple with n outputs output-1 ... output-n and one input 'input'"
  (declare (ignore environment))
  (if (<= output-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: output-count must be greater than 0: ~a"
       :format-arguments (list name output-count)))
  (let ((cur-input nil) (outputs (cl-synthesizer-macro-util:make-keyword-list "output" output-count)))
    (list
     :inputs (lambda () '(:input))
     :outputs (lambda () outputs)
     :get-output (lambda (output) (declare (ignore output)) cur-input)
     :update (lambda (&key input) (setf cur-input input)))))

