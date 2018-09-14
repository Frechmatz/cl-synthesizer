(in-package :cl-synthesizer)

(defun make-environment (&key (sample-rate 44100))
    (list
     :sample-rate sample-rate
     :output-directory "/Users/olli/"
     :audio-device (list
		    :symbol-name "SPEAKER-CL-OUT123"
		    :package-name "CL-SYNTHESIZER-DEVICE-SPEAKER"
		    :init-args (list :channel-count 2 :driver "coreaudio"))
     :midi-device (list
		   :symbol-name "MIDI-DEVICE"
		   :package-name "CL-SYNTHESIZER-DEVICE-MIDI"
		   :init-args nil)))
