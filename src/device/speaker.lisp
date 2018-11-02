;;
;;
;; A Speaker Device based on cl-out123
;;
;;
(in-package :cl-synthesizer-device-speaker)

(defun speaker-cl-out123 (name environment &key channel-count driver (buf-length-frames 1000) (v-peak 5.0))
  "Creates a speaker device using the \"cl-out123\" package to
    push audio data to a system speaker driver. The :update function as exposed by
    the device is blocking. This means that when the maximum buffer size
    has been reached, the function will not return until the speaker driver
    has accepted the buffer. This behaviour can be used to synchronize the
    synthesizer. The device has a latency of about 300-400ms.
    The function has the following arguments:
  <ul>
    <li>name A name.</li>
    <li>environment The synthesizer environment.</li>
    <li>:channel-count Number of output channels.</li>
    <li>:driver Driver to be used, for example \"coreaudio\".</li>
    <li>:v-peak Optional peak voltage. The inputs of the device will be normalized
	to -1.0 ... 1.0 according to v-peak.</li>
    <li>:buf-length-frames Number of frames to be buffered until the audio data is
	pushed to the driver.</li>
  </ul>
  The :update function of the device must be called with keyword arguments :channel-1 ... :channel-n,
  where n is the number of channels. In a stereo setup left is represented by :channel-1 and 
  right by :channel-2
  The current buffer of the device is flushed when the :shutdown function is called."
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: channel-count must be greater than 0: ~a"
       :format-arguments (list name channel-count)))
  (let ((inputs nil)
	(out nil)
	(buffer-pos 0)
	(buffer (make-array
		 (* channel-count buf-length-frames)
		 :element-type 'single-float :adjustable nil)))
    (dotimes (i channel-count)
      (push (cl-synthesizer-macro-util:make-keyword "channel" i) inputs))
    (setf inputs (reverse inputs))
    (flet ((init-out ()
	     (if (not out)
		 (progn
		   (setf out (make-instance 'cl-out123:output))
		   (cl-out123:connect out :driver driver)
		   (cl-out123:start
		    out
		    :rate (getf environment :sample-rate)
		    :channels channel-count
		    :encoding :float))))
	   (flush-buffer (force)
	     (if (or force (>= buffer-pos (length buffer)))
		 (progn
		   (cl-out123:play out buffer buffer-pos)
		   (setf buffer-pos 0)))))
      (list
       :inputs (lambda() inputs)
       :outputs (lambda() nil)
       :get-output (lambda(socket) (declare (ignore socket)) nil)
       :update (lambda (&rest args)
		 (init-out)
		 (flush-buffer nil)
		 (dolist (input inputs)
		   (let ((value (getf args input)))
		     (if (not value)
			 (setf value 0.0))
		     (setf (aref buffer buffer-pos)
			   ;; convert to -1.0 ... +1.0
			   (coerce (/ value v-peak) 'single-float))
		     (setf buffer-pos (+ 1 buffer-pos))) c))
       :shutdown (lambda ()
		   (flush-buffer t)
		   (cl-out123:disconnect out)
		   nil)))))
