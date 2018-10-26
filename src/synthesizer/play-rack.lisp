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

(defun make-audio-getter (rack sockets)
  (let ((map nil) (cur-channel 0) (get-output (getf rack :get-output)))
    (dolist (socket sockets)
      (let ((cur-socket socket))
	(push (lambda () (funcall get-output cur-socket)) map)
	(push (cl-synthesizer-macro-util:make-keyword "channel" cur-channel) map)
	(setf cur-channel (+ 1 cur-channel))))
    (lambda ()
      (mapcar (lambda(item) (if (keywordp item) item (funcall item))) map))))

(defun make-midi-getter (device socket)
  (let ((update (getf device :update)) (get-output (getf device :get-output)))
    (lambda ()
      (funcall update)
      (list socket (funcall get-output nil)))))

(defun play-rack (rack duration-seconds &key (attach-audio nil) (audio-output-sockets nil)
					    (attach-midi nil) (midi-input-socket nil))
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
  (let* ((environment (getf rack :environment))
	 (audio-device nil) (audio-getter nil)
	 (midi-device nil) (midi-getter nil))
    (if (and attach-audio audio-output-sockets)
	(progn
	  ;; todo channel-count nicht aus environment
	  (setf audio-device (make-audio-device "SPEAKER" environment))
	  (setf audio-getter (make-audio-getter rack audio-output-sockets))))
    (if (and attach-midi midi-input-socket)
	(progn
	  (setf midi-device (make-midi-device "MIDI" environment))
	  (setf midi-getter (make-midi-getter midi-device midi-input-socket))))
    (let ((f (getf rack :update)))
      (dotimes (i (* duration-seconds (getf environment :sample-rate)))
	(let ((input (if midi-device (funcall midi-getter) nil)))
	  (apply f input)
	  (if audio-device
	      (apply (getf audio-device :update) (funcall audio-getter))))))
    (funcall (getf rack :shutdown))
    (if audio-device
	(funcall (getf audio-device :shutdown)))
    (if midi-device
	(funcall (getf midi-device :shutdown)))
    "DONE"))

