(in-package :cl-synthesizer)

(defun make-audio-device (name environment)
  (let ((settings (getf environment :audio-device)))
    (if settings
	(make-device
	 name
	 environment
	 settings)
	(error (format nil "Audio device requested but not configured by environment")))))

(defun make-midi-device (name environment)
  (let ((settings (getf environment :midi-device)))
    (if settings
	(make-device
	 name
	 environment
	 settings)
	(error (format nil "MIDI device requested but not configured by environment")))))

(defun play-rack (rack duration-seconds &key (attach-speaker nil) (attach-midi nil))
  "A utility function that \"plays\" the rack by consecutively calling its update function
    for a given number of \"ticks\". The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
	<li>duration-seconds Duration in seconds of how long to play the rack. If for
	    example the duration is 2 seconds and the sample rate of the rack as declared
	    by its environment is 44100, then the update function of the rack will be called 88200 times.</li>
	<li>:attach-speaker If t then the audio device as declared by the environment property :audio-device 
            is instantiated and attached to the rack.</li>
	<li>:attach-midi If t then the MIDI device as declared by the environment property :midi-device
            is instantiated and attached to the rack.</li>
    </ul>
    The current implementation of the play-rack function assumes that an audio device is blocking."
  (let* ((environment (getf rack :environment)))
    (if attach-speaker
	(attach-audio-device rack #'make-audio-device))
    (if attach-midi
	(attach-midi-in-device rack #'make-midi-device))
    (let ((f (getf rack :update)))
      (dotimes (i (* duration-seconds (getf environment :sample-rate)))
	(funcall f nil)))
    (funcall (getf rack :shutdown))
    "DONE"))

