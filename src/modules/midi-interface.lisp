(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Module using CoreMidi
;;
;; Work in progress
;;

(defun midi-interface (environment)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((current-output 0)
	 (event-logger (funcall (getf environment :register-event) (format nil "MIDI-EVENT")))
	 (event-queue (queues:make-queue :simple-cqueue))
	 (converter (cl-synthesizer-core:linear-converter :cv-min 0 :cv-max 127 :f-min 0 :f-max 4.9)))
    (midi:initialize)
    (midi:set-midi-callback
     (midi:get-source 0)
     :cc ;; for now listen to all control change events
     (lambda (chan control value)
       (queues:qpush event-queue (list chan control value))
       (funcall event-logger)))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '())
     :outputs (lambda () '(:out-1))
     :get-output (lambda (output)
		   (declare (ignore output))
		   ;; TODO: Minimize expensive queue calls
		   (let ((e (queues:qpop event-queue)))
		     (if e (setf current-output (funcall (getf converter :get-frequency) (third e)))))
		   current-output)
     :update (lambda () nil))))


     
