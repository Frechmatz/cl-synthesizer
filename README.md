cl-synthesizer
==============

An experimental modular audio synthesizer implemented in Common Lisp.

Work in progress...

Rack
----

**cl-synthesizer:make-rack** &key environment

**cl-synthesizer:add-module** rack module-name module-fn &rest args

**cl-synthesizer:add-patch** rack source-rm-name source-output-socket destination-rm-name destination-input-socket

**cl-synthesizer:update** rack

Process a tick

**cl-synthesizer:play-rack** rack duration-seconds &key

**cl-synthesizer:make-environment** &key

**cl-synthesizer:get-environment** rack

**cl-synthesizer:get-module** rack name

Returns a module (as added via add-module) or nil

**cl-synthesizer:get-patch** rack module-name socket-type socket

Returns values (name module socket) of connected module

**cl-synthesizer:get-line-out-adapter** rack

**cl-synthesizer:get-midi-in-adapter** rack

**cl-synthesizer:add-hook** rack hook

Add a hook to a rack. A hook is called each time after the rack has updated its state. A hook consists a property list with the following keys:

*   :update A function with no arguments that is called after the rack has updated its state.
*   :shutdown A function with no arguments that is called when the rack is shutting down.

Hooks must not modify the rack.

Modules
-------

**cl-synthesizer-modules-vco:vco-linear** name environment &key base-frequency f-max v-peak cv-max

Creates a Voltage Controlled Oscillator module with linear characteristic. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :cv-max The frequency control voltage which represents the maximum frequency of the oscillator.
*   :base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.
*   :f-max See vco-base.
*   :v-peak See vco-base.
*   :duty-cycle See vco-base.

The module has the following inputs:

*   :cv Frequency control voltage. For frequency calculation the absolute value of the control voltage is used. The control voltage is clipped at :cv-max.

For the outputs of the module see vco-base.

**cl-synthesizer-modules-vco:vco-exponential** name environment &key base-frequency f-max v-peak

Creates a Voltage Controlled Oscillator module with 1V/Octave characteristic. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.
*   :f-max See vco-base.
*   :v-peak See vco-base.
*   :duty-cycle See vco-base.

The module has the following inputs:

*   :cv Frequency control voltage. For a given base-frequency of 440Hz a control voltage of 1.0 results in a frequency of 880Hz and a control voltage of -1.0 results in a frequency of 220Hz.

For the outputs of the module see vco-base.

**cl-synthesizer-modules-vca:vca** name environment &key cv-max

Creates a Voltage Controlled Amplifier/Attenuator module. The VCA multiplies an incoming signal with a factor of 0..1. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :cv-max The value of the effective amplification control voltage that represents the maximum amplification of 1.0.
*   :initial-gain An offset that is added to the amplification control voltage. Default value is 0.0.

The module has the following inputs:

*   :cv Amplification control voltage.
*   :input Input signal to be amplified. The amplitude of this voltage is unknown to the VCA. It can have any value.
*   :gain An offset that is added to the amplification control voltage.

The effective amplification voltage is v = :cv + :gain + :initial-gain, where 0.0 <= v <= :cv-max. The module has the following outputs:

*   :output-linear Amplified input signal with linear amplification characteristic.
*   :output-exponential Amplified input signal with exponential amplification characteristic.

Examples can be found under /src/modules/vca/

**cl-synthesizer-modules-multiple:multiple** name environment &key output-count

Creates a Multiple module. A multiple mirrors one input to n outputs. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :output-count The number of outputs.

The module has the following inputs:

*   :input The input signal to be mirrored to the outputs.

The module has outputs :output-1 ... :output-n.

**cl-synthesizer-modules-midi-sequencer:midi-sequencer** name environment &key events

Creates a Midi-Sequencer module. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :events A list of Midi event declarations. Each declaration entry is a property list with the following keys:
    *   :timestamp-milli-seconds Point of time when events are to be fired.
    *   :midi-events List of Midi events to be fired. For the format of a single Midi event see /src/midi/event.lisp.

The module has no inputs. The module has one output socket :midi-events.

**cl-synthesizer-modules-midi-interface:midi-interface** name environment &key

Creates a MIDI interface module. The module dispatches MIDI-Note events to so called voices where each voice is represented by a control-voltage and a gate signal. The module supports the mapping of MIDI CC-Events to arbitary output sockets. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :voice-count The number of voices to be exposed by the module. Each voice consists of the following output sockets:
    *   :GATE-n The gate signal, for example :GATE-1 n = 1..voice-count.
    *   :CV-n A control voltage representing the note number, for example :CV-1. n = 1..voice-countThe default voice count is 1.
*   :channel Optional MIDI channel to which note events must belong. By default the channel is ignored. This setting does not effect the evaluation of CC-Events that are handled by controllers. Controllers must implement channel filtering on their own.
*   :note-number-to-cv An optional function that is called with a MIDI note number and returns a control-voltage. The default implementation is (lambda (note-number) (/ note-number 12)))
*   :play-mode
    *   :PLAY-MODE-POLY Polyphonic play mode. Incoming note events will be dispatched to "available" voices, where a voice is available when it meets certain criteria. These criteria are defined and implemented by the cl-synthesizer-midi-voice-manager:voice-manager package.
    *   :PLAY-MODE-UNISONO Monophonic play mode. All voices exposed by the module are set to the current "active" note. Notes are stacked. When a note is released, the voice outputs switch to the previous note. This logic is also implemented by the cl-synthesizer-midi-voice-manager:voice-manager package.The default value is :PLAY-MODE-POLY
*   :cv-gate-on The "Gate on" control voltage. The default value is 5.0
*   :cv-gate-off The "Gate off" control voltage. The default value is 0.0
*   :force-gate-retrigger If t then in :PLAY-MODE-UNISONO play mode each note event will cause a retriggering of the gate signal. Otherwise the gate signal will stay on when it is already on.
*   :controllers Controllers can be used to declare additional output sockets that are exposed by the module. The controllers argument consists of a list of property lists with the following keys:
    *   :socket A keyword that defines the output socket to be exposed by the modules.
    *   :handler A property list that defines the keys
        *   :update A function that is called with the MIDI events passed to the update function of the module.
        *   :get-output A function with no arguments that returns the current value of the controller.For typical use cases refer to cl-synthesizer-midi:relative-cc-handler

Gate transitions are implemented as follows:

*   In :PLAY-MODE-POLY play mode each incoming note causes that the gate signal of the assigned voice switches to On. If the gate signal of the assigned voice is already On (this happens when the available voices are exhausted and a voice is "stolen") then the gate signal switches to Off for the duration of one system tick and then to On again.
*   In :PLAY-MODE-UNISONO play mode incoming notes are stacked. The first note causes the gate signal to switch to On. Further "nested" note-on events only result in a change of the CV output but the gate signal will stay On. This behaviour can be overridden with the :force-gate-retrigger parameter.

The module has the following inputs:

*   :midi-events A list of MIDI events.

The module has the following outputs:

*   :GATE-1 ... :GATE-n
*   :CV-1 ... :CV-n
*   Outputs as defined by controllers

Example:

    
        (cl-synthesizer:add-module
            rack
            "MIDI-IFC"
            #'cl-synthesizer-modules-midi-interface:midi-interface
            :voice-count 2
            :play-mode :PLAY-MODE-POLY
    	:controllers
    	(list
    	    (list :socket
                      :controller-1
    	          :handler
                          (cl-synthesizer-midi:relative-cc-handler
    		      cl-synthesizer-vendor:*arturia-minilab-mk2*
    		      (list
                              (list
                                  :controller-id :ENCODER-1
                                  :weight 0.01
    			      :cv-initial 2.5
    			      :cv-min 0
    		              :cv-max 5))))))
        

**cl-synthesizer-modules-envelope:envelope** name environment &key segments

Creates an envelope generator module. An envelope consists of a list of segments where each segment defines rules how to behave. The module generates linear envelopes. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :segments The segments of the envelope. Each segment consists of a property list with the following keys:
    
    *   :duration-ms Optional duration of the segment in milli-seconds. The effective duration depends on the sample rate as specified by the environment.
    *   :target-cv Optional target voltage to which the segment shall climb.
    *   :required-gate-state One of :on :off :ignore
    *   :duration-controller Declares a controller with which the duration of the segment can be modulated.
    *   :target-cv-controller Declares a controller with which the target voltage of the segment can be modulated.
    
    A Controller represents an external input that is exposed by the module and can be used to modulate a certain property of the segment. External input values are mapped by a linear function to the actual values that are processed by the segment. Controllers are represented as property lists with the following keys:
    *   :socket A keyword that defines the input socket that will be exposed by the envelope module and to which other modules can be connected.
    *   :input-min The minimum input value of the socket.
    *   :input-max The maximum input value of the socket.
    *   :output-min The minimum target value of the mapping.
    *   :output-max The maximum target value of the mapping.Clipping is generally not applied except for cases such as a negative segment duration. Controller inputs are always offsets that are added to the initial value as provided by :duration-ms or :target-cv. Controller inputs do not affect the behaviour of the currently active segment.
*   :gate-threshold An optional threshold which defines the minimum input value of the :gate input that is interpreted as gate on. The default value is 4.9

The module has the following inputs:

*   :gate The gate signal as provided for example by a MIDI sequencer. If the gate switches from :off to :on the output voltage is reset to 0.0 and the module switches to the first segment.
*   Inputs as defined by segment controllers.

The module has the following outputs:

*   :cv The current value of the envelope. The initial value is 0.0

Example:

    
        (cl-synthesizer:add-module rack "ADSR" #'cl-synthesizer-modules-envelope:envelope
    			       :segments '((:duration-ms 200 :target-cv 5 :required-gate-state :on
    					    :duration-controller
    					    (:socket :attack-duration
    					     :input-min -5.0
    					     :input-max 5.0
    					     :output-min -1000
    					     :output-max 1000))
    					   (:duration-ms 100 :target-cv 3 :required-gate-state :on)
    					   (:required-gate-state :on)
    					   (:duration-ms 300 :target-cv 0 :required-gate-state :off)))
        

**cl-synthesizer-modules-fixed-output:fixed-output** name environment &key value

Creates a module with a fixed output value. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :value The value of the module output.
*   :output-socket Optional keyword that declares the output socket identifier to be exposed by the module. The default value is :out

The module has no inputs. The module has one output socket according to the :output-socket argument.

Monitor
-------

**cl-synthesizer-monitor:add-monitor** rack monitor-backend socket-mappings &rest additional-backend-args

Adds a monitor to a rack. A monitor is a high-level Rack hook that collects module states (input/output sockets) and passes them to a monitor backend. A monitor backend can for example be a Wave-File-Writer. The function has the following arguments:

*   rack The rack.
*   monitor-backend A function that instantiates the monitor backend. This function is called with the following arguments:
    
    *   name A name.
    *   environment The synthesizer environment.
    *   output-keywords A list of keywords declaring the keyword parameters with which the monitor backend update function will be called.
    *   additional-backend-args Any additional keyword parameters as passed to the monitor function. These parameters can be used to initialize backend specific properties such as a filename.
    
    The function must return a property list with the following keys:
    *   :update A function that is called after each tick of the rack. It is called with keyword parameters as declared by the output-keywords argument described above.
    *   :shutdown An optional function with no arguments that is called when the rack shuts down.
*   socket-mappings Declares a list of mappings of specific sockets of specific modules to keyword parameters that will be passed to the update function of the backend. Each mapping entry has the following format:
    *   key Keyword to be used as keyword parameter when the backend update function is called, for example :channel-1. For now this key must be one that is supported by the backend. For example the Wave-File-Writer backend only understands :channel-n keys. This might be changed in a later point of time.
    *   module-name Name of the module from which the state of a certain input/output socket is to be retrieved, for example "ADSR"
    *   socket-type Defines if the value of an input-socket is requested or the value of an output-socket. Must be :input-socket or :output-socket
    *   socket A keyword that identifies one of the input/output sockets as provided by the module, for example :cv
*   &rest additional-backend-args Optional keyword arguments to be passed to the backend instantiation function.

Example using the wave-file-handler monitor backend:

    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "LFO" :output-socket :saw)
           (:channel-2 "ADSR" :output-socket :cv)
           (:channel-3 "LINE-OUT" :input-socket :channel-1))
         :filename "trace.wav")
        

Adds a monitor to a rack. A monitor is a high-level Rack hook that collects module states (input/output sockets) and passes them to a monitor backend. A monitor backend can for example be a Wave-File-Writer. The function has the following arguments:

*   rack The rack.
*   monitor-backend A function that instantiates the monitor backend. This function is called with the following arguments:
    
    *   name A name.
    *   environment The synthesizer environment.
    *   output-keywords A list of keywords declaring the keyword parameters with which the monitor backend update function will be called.
    *   additional-backend-args Any additional keyword parameters as passed to the monitor function. These parameters can be used to initialize backend specific properties such as a filename.
    
    The function must return a property list with the following keys:
    *   :update A function that is called after each tick of the rack. It is called with keyword parameters as declared by the output-keywords argument described above.
    *   :shutdown An optional function with no arguments that is called when the rack shuts down.
*   socket-mappings Declares a list of mappings of specific sockets of specific modules to keyword parameters that will be passed to the update function of the backend. Each mapping entry has the following format:
    *   key Keyword to be used as keyword parameter when the backend update function is called, for example :channel-1. For now this key must be one that is supported by the backend. For example the Wave-File-Writer backend only understands :channel-n keys. This might be changed in a later point of time.
    *   module-name Name of the module from which the state of a certain input/output socket is to be retrieved, for example "ADSR"
    *   socket-type Defines if the value of an input-socket is requested or the value of an output-socket. Must be :input-socket or :output-socket
    *   socket A keyword that identifies one of the input/output sockets as provided by the module, for example :cv
*   &rest additional-backend-args Optional keyword arguments to be passed to the backend instantiation function.

Example using the wave-file-handler monitor backend:

    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "LFO" :output-socket :saw)
           (:channel-2 "ADSR" :output-socket :cv)
           (:channel-3 "LINE-OUT" :input-socket :channel-1))
         :filename "trace.wav")
        

Devices
-------

**cl-synthesizer-device-speaker:speaker-cl-out123** name environment &key channel-count driver

Creates a speaker device. The device is using the cl-out123 package to push audio data to a system speaker driver. The :update function as exposed by the device is blocking. This means that when the maximum buffer size has been reached, the function will not return until the speaker driver has accepted the buffer. This behaviour can be used to synchronize the synthesizer. The device has a latency of about 300-400ms and therefore cannot really be used for real-time play using a Midi-Controller. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :channel-count Number of output channels.
*   :driver Driver to be used, for example "coreaudio".
*   :v-peak Optional peak voltage. The inputs of the device will be normalized to -1.0 ... 1.0 according to v-peak. Incoming voltages will not be clipped. The default value is 5.0.
*   :buf-length-frames Number of frames to be buffered until the audio data is pushed to the driver. Default value is 1000.

The device has the following inputs:

*   :channel-1 ... :channel-n In a stereo setup left is represented by :channel-1 and right by :channel-2

The module has no outputs. The current buffer is flushed when the :shutdown function as exposed by the device is being called.

**cl-synthesizer-device-midi:midi-device** name environment

Readme generated 2018-09-19 22:17:29