(in-package :cl-synthesizer)

(defun make-environment (&key (sample-rate 44100) (channel-count 2) (home-directory nil))
  "Creates an environment. The environment defines properties such as
    the sample rate of the rack and the number of its audio output channels.
    An enviroment is a property list with the following keys:
    <ul>
	<li>:sample-rate Sample rate of the synthesizer.</li>
	<li>:channel-count The number of line-out sockets exposed to rack modules and an audio output device.</li>
	<li>:home-directory The base output directory for wave files etc. Default value is the home directory
        of the current user.</li>
	<li>:audio-device The audio device to be instantiated when audio output is required. For the format
            of this argument see function make-device.</li>
	<li>:midi-device The MIDI device to be instantiated when MIDI input is required. For the format
            of this argument see function make-device.</li>
    </ul>"
    (list
     :sample-rate sample-rate
     :channel-count channel-count
     :home-directory (if (not home-directory) (user-homedir-pathname) home-directory) 
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

(defun prepare-init-args (environment initargs)
  "Resolves init arguments that are defined as functions by calling the 
  corresponding function with the environment as argument."
  (let ((prepared nil))
    (dotimes (i (length initargs))
      (let ((item (nth i initargs)))
	(if (functionp item)
	    (push (funcall item environment) prepared)
	    (push item prepared))))
    (reverse prepared)))

(defun make-device (name environment device-settings)
  "Creates a device. The function has the following arguments:
    <ul>
	<li>name Name of the device.</li>
	<li>environment The synthesizer environment.</li>
	<li>device-settings The device settings consist of a property list with the following keys:
	    <ul>
		<li>:symbol-name Symbol name of the device instantiation function, for example \"SPEAKER-CL-OUT123\"</li>
		<li>:package-name Package name of the device instantiation function, for example \"CL-SYNTHESIZER-DEVICE-SPEAKER\"</li>
		<li>:init-args A list of additional arguments to be passed to the device instantiation function. An
		    argument may consist of a function. In this case the value of the argument to be passed to the
		    device instantiation function will be evaluated by calling the given function with the environment as
		    parameter. Example: <code>(:channel-count (lambda (environment) (getf environment :channel-count))
		    :driver \"coreaudio\"))</code></li>
	    </ul>
	</li>
    </ul>
    The device instantiation function is called with the following arguments:
    <ul>
	<li>name Name of the device.</li>
	<li>environment The synthesizer environment.</li>
	<li>Any additional arguments as declared by :init-args</li>
    </ul>
    If the device represents a MIDI input device then the device instantiation function must return a property
    list with the following keys:
    <ul>
	<li>:get-output A function that returns the current output of the underlying device as
	a list of Midi-Events.</li>
	<li>:shutdown An optional shutdown function that is called when the rack is shutting down.</li>
    </ul>
    If the device represents an Audio output device then the device instantiation function must return a property
    list with the following keys:
    <ul>
	<li>:update Function that is called with keywords parameters :channel-1 ... :channel-n in order to
	push audio data to the underlying device.</li>
	<li>:shutdown An optional shutdown function that is called when the rack is shutting down.</li>
    </ul>"
  (let ((symbol-name (getf device-settings :symbol-name))
	(package-name (getf device-settings :package-name)))
    (if (not symbol-name)
	(error "Device instantiation failed. :symbol-name is a mandatory device configuration property")) 
    (if (not package-name)
	(error "Device instantiation failed. :package-name is a mandatory device configuration property")) 
    (format t "~%Instantiating device ~a::~a..." symbol-name package-name)
    (let ((ctor (find-symbol symbol-name package-name)))
      (if ctor
	  (apply ctor name environment (prepare-init-args environment (getf device-settings :init-args)))
	  (error (format nil "Device instantiation failed. Symbol ~a::~a not found" symbol-name package-name))))))
