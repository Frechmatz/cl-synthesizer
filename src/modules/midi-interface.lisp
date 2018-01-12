(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;


(defun midi-interface (environment)
  (declare (ignore environment))
  (let ((current-output 0)
	(converter (cl-synthesizer-core:linear-converter :input-min 0 :input-max 127 :output-min 0 :output-max 4.9)))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:midi-event))
     :outputs (lambda () '(:out-1))
     :get-output (lambda (output)
		   (declare (ignore output))
		   current-output)
     :update (lambda (&key (midi-event nil))
	       (if midi-event (setf current-output (funcall (getf converter :input-to-output) (third midi-event))))))))

