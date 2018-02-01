;;
;;
;; A MIDI Device based on CoreMidi 
;;
;; Work in progress
;;

(in-package :cl-synthesizer-device-midi)

(defparameter *source* 1) ;; 0 == VMPK

(defun midi-device (name environment)
  (declare (ignore name))
  (declare (ignore environment))
  (let ((event-queue (queues:make-queue :simple-cqueue)))
    (midi:initialize)
    (midi:set-midi-callback
     (midi:get-source *source*)
     :cc ;; for now listen to all control change events
     (lambda (chan control value)
       (format t "[CC] Channel: ~d  Control: ~d  Value: ~d~%" chan control value)       
       (queues:qpush event-queue (list :cc chan control value))))
    (midi:set-midi-callback
     (midi:get-source *source*)
     :note-on
     (lambda (chan note vel)
       ;; [NOTE-ON] Channel: 1  Notenum: 61  Velocity: 15
       (format t "[NOTE-ON ] Channel: ~d  Notenum: ~d  Velocity: ~d~%" chan note vel)
       (queues:qpush event-queue (list :note-on chan note vel))))
    (midi:set-midi-callback
     (midi:get-source *source*)
     :note-off
     (lambda (chan note vel)
       ;; [NOTE-OFF] Channel: 1  Notenum: 61  Velocity: 0
       (format t "[NOTE-OFF] Channel: ~d  Notenum: ~d  Velocity: ~d~%" chan note vel)
       (queues:qpush event-queue (list :note-off chan note vel))))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '())
     :outputs (lambda () '(:midi-output))
     :get-output (lambda (output)
		   (declare (ignore output))
		   (queues:qpop event-queue))
     :update (lambda () nil))))


     