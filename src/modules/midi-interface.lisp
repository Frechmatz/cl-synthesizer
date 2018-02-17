(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;

;;
;; MIDI Module
;;

(defun midi-interface (name environment)
  (let ((current-controller 0)
	(current-gate 0)
	(current-cv-oct 0)
	(controller-converter (cl-synthesizer-core:linear-converter :input-min 0 :input-max 127 :output-min 0 :output-max 4.9))
	(gate-on-event (funcall (getf environment :register-event) name "GATE-ON"))
	(gate-off-event (funcall (getf environment :register-event) name "GATE-OFF")))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:midi-event))
     :outputs (lambda () '(:gate :cv-oct :out-1))
     :get-output (lambda (output)
		   (cond
		     ((eq output :gate) current-gate)
		     ((eq output :cv-oct) current-cv-oct)
		     ((eq output :out-1) current-controller)
		     (t (error (format nil "Unknown input ~a requested from ~a" output name)))))
     :update (lambda (&key (midi-event nil))
	       (if midi-event
		   (let ((event-type (first midi-event)))
		     (cond
		       ((eq event-type :cc)
			(setf current-controller
			      (funcall (getf controller-converter :input-to-output) (fourth midi-event))))
		       ((eq event-type :note-on)
			(funcall gate-on-event)
			(setf current-gate 5.0)
			(let ((note-number (third midi-event)))
			  (setf current-cv-oct (/ note-number 12))
			  (format t "cv-oct: ~a~%" current-cv-oct)
			  )
			)
		       ((eq event-type :note-off)
			(funcall gate-off-event)
			(setf current-gate 0)))
		     (format t "Gate: ~a CV-Oct: ~a Controller: ~a~%" current-gate current-cv-oct current-controller)))))))
