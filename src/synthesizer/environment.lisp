(in-package :cl-synthesizer)

(defun make-environment (&key (sample-rate 44100) (channel-count 2) (output-directory "/Users/olli/"))
  "Creates an environment. The environment defines properties such as
    the sample rate of the rack and the number of its audio output channels.
    For now the function does not support all settings of the environment. Some
    are hard coded. An enviroment is a property list with the following keys:
    <ul>
	<li>:sample-rate Sample rate of the synthesizer.</li>
	<li>:channel-count The number of line-out sockets exposed to rack modules and an audio output device.</li>
	<li>:output-directory The base output directory for wave files etc.</li>
	<li>:audio-device The audio device to be instantiated when audio output is required. For more details see
	    src/synthesizer/environment.lisp.</li>
	<li>:midi-device The MIDI device to be instantiated when MIDI input is required. For more details see
	    src/synthesizer/environment.lisp.</li>
    </ul>"
    (list
     :sample-rate sample-rate
     :channel-count channel-count
     :output-directory output-directory
     :audio-device (list
		    :symbol-name "SPEAKER-CL-OUT123"
		    :package-name "CL-SYNTHESIZER-DEVICE-SPEAKER"
		    :init-args (list
				:channel-count (lambda (environment) (getf environment :channel-count))
				:driver "coreaudio"))
     :midi-device (list
		   :symbol-name "MIDI-DEVICE"
		   :package-name "CL-SYNTHESIZER-DEVICE-MIDI"
		   :init-args nil)))
