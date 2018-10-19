;;
;;
;; A MIDI Device based on CoreMidi 
;;
;;

(in-package :cl-synthesizer-device-midi)

(defun midi-device (name environment &key (source-index 1))
  "Creates a MIDI device using the \"coremidi\" package to receive MIDI events.
    The function has the following arguments:
    <ul>
	<li>name A name.</li>
	<li>environment The synthesizer environment.</li>
	<li>:source-index The source index argument as required by the midi:get-source function
            of the coremidi package.</li>
    </ul>
    The :get-output function returns a list of MIDI events. The events are sorted by
    their timestamp in ascending order, which means that the first event of the list
    is the \"oldest\" one."
  (declare (ignore name environment))
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((event-queue (queues:make-queue :simple-cqueue))
	(cur-midi-events nil))
    (midi:initialize)
    (midi:set-midi-callback
     (midi:get-source source-index)
     :cc ;; for now listen to all control change events
     (lambda (chan control value)
       ;;(format t "[CC] Channel: ~d  Control: ~d  Value: ~d~%" chan control value)
       (queues:qpush event-queue (cl-synthesizer-midi-event:make-control-change-event chan control value))))
    (midi:set-midi-callback
     (midi:get-source source-index)
     :note-on
     (lambda (chan note vel)
       ;; [NOTE-ON] Channel: 1  Notenum: 61  Velocity: 15
       ;;(format t "[NOTE-ON ] Channel: ~d  Notenum: ~d  Velocity: ~d~%" chan note vel)
       (queues:qpush event-queue (cl-synthesizer-midi-event:make-note-on-event chan note vel))))
    (midi:set-midi-callback
     (midi:get-source source-index)
     :note-off
     (lambda (chan note vel)
       ;; [NOTE-OFF] Channel: 1  Notenum: 61  Velocity: 0
       ;;(format t "[NOTE-OFF] Channel: ~d  Notenum: ~d  Velocity: ~d~%" chan note vel)
       (queues:qpush event-queue (cl-synthesizer-midi-event:make-note-off-event chan note vel))))
    (list
     :outputs (lambda() '(:midi-events))
     :inputs (lambda() nil)
     :update (lambda ()
	       (let ((events nil))
		 (loop
		    (let ((e (queues:qpop event-queue)))
		      (if (not e)
			  (return)
			  (setf events (push e events)))))
		 (setf cur-midi-events (nreverse events))))
     :get-output (lambda (output)
		   "Returns list of MIDI-Events where the oldest one is the first entry of the list"
		   ;;(declare (optimize (debug 3) (speed 0) (space 0)))
		   (declare (ignore output))
		   cur-midi-events))))


     
