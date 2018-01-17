(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;

;;
;;
;; Note Mapping
;;
;;

;; http://subsynth.sourceforge.net/midinote2freq.html
(defun note-frequency-table ()
  (let ((arr (make-array 128)) (a 440))
    (dotimes (x 128)
      (let ((frequency
	     (* (/ a 32) (expt 2 (/ (+ x -9) 12)))))
	(setf (aref arr x) frequency)))
    arr))

(defun frequency-cv-table (frequency-table)
  (let* ((arr (make-array 128))
	 (converter (cl-synthesizer-core:linear-converter :input-min 0 :input-max 5.0 :output-min 0 :output-max 13000))
	 (fn (getf converter :output-to-input)))
    (dotimes (x 128)
      (setf (aref arr x) (funcall fn (elt frequency-table x))))
    arr))

;;
;; MIDI Module
;;

(defun midi-interface (environment)
  (declare (ignore environment))
  (let ((current-controller 0)
	(current-gate 0)
	(current-cv 0)
	(note-to-cv-table (frequency-cv-table (note-frequency-table)))
	(controller-converter (cl-synthesizer-core:linear-converter :input-min 0 :input-max 127 :output-min 0 :output-max 4.9)))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () '(:midi-event))
     :outputs (lambda () '(:gate :cv :out-1))
     :get-output (lambda (output)
		   (cond
		     ((eq output :gate) current-gate)
		     ((eq output :cv) current-cv)
		     ((eq output :out-1) current-controller)
		     (t (error (format nil "Unknown input ~a requested from MIDI-INTERFACE" output)))))
     :update (lambda (&key (midi-event nil))
	       (if midi-event
		   (let ((event-type (first midi-event)))
		     (cond
		       ((eq event-type :cc)
			(setf current-controller
			      (funcall (getf controller-converter :input-to-output) (fourth midi-event))))
		       ((eq event-type :note-on)
			(setf current-gate 5.0)
			(setf current-cv
			      (elt note-to-cv-table (third midi-event))))
		       ((eq event-type :note-off)
			(setf current-gate 0)))
		     (format t "Gate: ~a CV: ~a Controller: ~a~%" current-gate current-cv current-controller)))))))
