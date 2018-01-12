;;
;;
;; A MIDI Device based on CoreMidi 
;;
;; Work in progress
;;

(in-package :cl-synthesizer-device-midi)

(defparameter *source* 1) ;; 0 == VMPK

(defun midi-device (environment)
  (declare (ignore environment))
  (let ((event-queue (queues:make-queue :simple-cqueue)))
    (midi:initialize)
    (midi:set-midi-callback
     (midi:get-source *source*)
     :cc ;; for now listen to all control change events
     (lambda (chan control value)
       (format t "[CC] Channel: ~d  Control: ~d  Value: ~d~%" chan control value)       
       (queues:qpush event-queue (list chan control value))))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '())
     :outputs (lambda () '(:midi-output))
     :get-output (lambda (output)
		   (declare (ignore output))
		   (queues:qpop event-queue))
     :update (lambda () nil))))


     
