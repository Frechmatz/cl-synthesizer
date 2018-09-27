cl-synthesizer
==============

An experimental modular audio synthesizer implemented in Common Lisp. Work in progress.

A synthesizer is represented by an instance of a Rack. A rack contains all the modules and the patches (wiring) between them. A rack also provides an interface for system specific devices such as MIDI and Audio. The implementation however does not depend on any system specific libraries such as CoreMidi or audio drivers.

**Example:**

    (defpackage :cl-synthesizer-rack-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-rack-example-1)
    
    (defparameter *attach-speaker* nil)
    
    (defun example ()
      "Modulate the frequency of a saw signal with a LFO."
      (let ((rack (cl-synthesizer:make-rack
    	       :environment
    	       (cl-synthesizer:make-environment))))
    
        (cl-synthesizer:add-module
         rack "LFO-1"
         #'cl-synthesizer-modules-vco:vco-linear
         :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    
        (cl-synthesizer:add-module
         rack "LFO-2"
         #'cl-synthesizer-modules-vco:vco-linear
         :base-frequency 2.0 :v-peak 1.0 :f-max 500 :cv-max 5)
    
        (cl-synthesizer:add-module
         rack "VCO-1"
         #'cl-synthesizer-modules-vco:vco-linear
         :base-frequency 440 :f-max 5000 :v-peak 5 :cv-max 5)
    
        (cl-synthesizer:add-module
         rack "VCO-2"
         #'cl-synthesizer-modules-vco:vco-linear
         :base-frequency 442 :f-max 5000 :v-peak 5 :cv-max 5)
        
        (cl-synthesizer:add-patch rack "LFO-1" :sine "VCO-1" :cv)
        (cl-synthesizer:add-patch rack "LFO-2" :sine "VCO-2" :cv)
        (cl-synthesizer:add-patch rack "VCO-1" :saw "LINE-OUT" :channel-1)
        (cl-synthesizer:add-patch rack "VCO-2" :saw "LINE-OUT" :channel-2)
    
        ;; Write LFO/VCO outputs to Wave-File
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "LFO-1" :output-socket :sine)
           (:channel-2 "LFO-2" :output-socket :sine)
           (:channel-3 "VCO-1" :output-socket :saw)
           (:channel-4 "VCO-2" :output-socket :saw))
         :filename "rack-example-1-vcos.wav")
        
        ;; Write LINE-OUT to Wave-File
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "LINE-OUT" :input-socket :channel-1)
           (:channel-2 "LINE-OUT" :input-socket :channel-2))
         :filename "rack-example-1.wav")
        
        rack))
    
    ;; Execute to run rack
    ;;(cl-synthesizer:play-rack (example) 5 :attach-speaker *attach-speaker*)
    
    
    

Installation
------------

TODO

API Reference
-------------

### Environment

**cl-synthesizer:make-environment** &key (sample-rate 44100) (channel-count 2) (home-directory nil)

Creates an environment. The environment defines properties such as the sample rate of the rack and the number of its audio output channels. An enviroment is a property list with the following keys:

*   :sample-rate Sample rate of the synthesizer.
*   :channel-count The number of line-out sockets exposed to rack modules and an audio output device.
*   :home-directory The base output directory for wave files etc. Default value is the home directory of the current user.
*   :audio-device The audio device to be instantiated when audio output is required. For the format of this argument see function make-device.
*   :midi-device The MIDI device to be instantiated when MIDI input is required. For the format of this argument see function make-device.

### Rack

**cl-synthesizer:make-rack** &key environment

Creates a rack. The function has the following arguments:

*   :environment The synthesizer environment.

A rack is initialized with the virtual modules "LINE-OUT" and "MIDI-IN" that represent the interface to so called devices. A device is a system specific implementation that provides audio output or integration of MIDI controllers. The "LINE-OUT" module exposes input sockets :channel-1 ... :channel-n where n is the channel-count as given by the :channel-count property of the environment. The "MIDI-IN" module exposes the output socket :midi-events which provides a list of midi-events as fired by a MIDI device. The devices to be used are declared by the environment properties :audio-device and :midi-device. The declared devices will only be instantiated when audio or MIDI are explicitly requested. Modules can be patched with MIDI/Audio input/output even if the environment does not declare device implementations or if the implementations are not supported by the current system.

* * *

**cl-synthesizer:add-module** rack module-name module-fn &rest args

Adds a module to a rack. The function has the following arguments:

*   rack The rack.
*   module-name Unique name of the module, for example "VCO-1". If the name is already used by another module an assembly-error is signalled.
*   module-fn A function that instantiates the module. This function is called by the rack with the following arguments:
    
    *   name Name of the module.
    *   environment The synthesizer environment.
    *   module-args Any additional arguments passed to add-module.
    
    The module instantiation function must return a property list with the following keys:
    *   :inputs A function with no arguments that returns a list of keywords that represent the input sockets to be exposed by the module.
    *   :outputs A function with no arguments that returns a list of keywords that represent the output sockets to be exposed by the module.
    *   :update A function that is called in order to update the values of the modules output sockets according to the values of its input sockets. The value of each input socket is represented by a keyword parameter.
    *   :get-output A function that is called in order to get the value of a specific output socket. The function is called with a keyword that identifies the output socket whose state is to be returned. The function must not modify the value of the given or any other output socket.
    *   :shutdown An optional function with no arguments that is called when the rack is shutting down.The input/output sockets exposed by the module are not buffered by the rack. Therefore the module should return either a quoted list or keep it in an internal variable. The module must not add or remove input/output sockets after it has been instantiated.
*   &rest args Arbitrary additional arguments to be passed to the module instantiation function. These arguments typically consist of keyword parameters.

* * *

**cl-synthesizer:add-patch** rack source-rm-name source-output-socket destination-rm-name destination-input-socket

Adds a patch to the rack. A patch is an unidirectional connection between an output socket of a source module and an input socket of a destination module. The rack supports cycles which means that an output socket of a module can be patched with one of its inputs (typically via multiple hops through other modules). The function has the following arguments:

*   rack The rack.
*   source-rm-name Name of the source module.
*   source-output-socket A keyword representing one of the output sockets of the source module.
*   destination-rm-name Name of the destination module.
*   destination-input-socket A keyword representing one of the input sockets of the destination module.

The rack signals an assembly-error in the following cases:

*   A module with the given source name does not exist.
*   A module with the given destination name does not exist.
*   The given source-output-socket is already connected with a module
*   The given source-output-socket is not exposed by the source module.
*   The given destination-input-socket is already connected with a module.
*   The given destination-input-socket is not exposed by the destination module.

* * *

**cl-synthesizer:update** rack

Updates the state of a rack by calling the update function of all its modules. The function has the following arguments:

*   rack The rack.

* * *

**cl-synthesizer:play-rack** rack duration-seconds &key (attach-speaker nil) (attach-midi nil)

* * *

**cl-synthesizer:get-environment** rack

Returns the environment of the rack.

* * *

**cl-synthesizer:get-module** rack name

Get a module of a rack. The function has the following arguments:

*   rack The rack.
*   name The name of the module

Returns the module (represented as property list) or nil if a module with the given name has not been added to the rack.

* * *

**cl-synthesizer:get-patch** rack module-name socket-type socket

Returns the destination module and input/output socket, to which a given source module and one if its input/output sockets is connected. The function has the following arguments:

*   rack The rack.
*   module-name Name of the source module.
*   socket-type :input-socket if the patch of an input socket is required or :output-socket for the patch of an output socket of the source module.
*   socket A keyword identifying an input or output socket of the source module.

The function returns nil if the source module does not exist or if the source module does not expose the given socket or if the given socket is not connected with a module. Otherwise it returns a list with the following entries:

*   name Name of the destination module.
*   module The destination module represented as a property list.
*   socket A keyword that identifies the input or output socket of the destination module. If the socket type of the source module is :input-socket then this keyword represents an output socket of the destination module. Otherwise it represents an input socket.

* * *

**cl-synthesizer:get-line-out-adapter** rack

* * *

**cl-synthesizer:get-midi-in-adapter** rack

* * *

**cl-synthesizer:add-hook** rack hook

Adds a hook to the rack. A hook is called each time after the rack has updated its state. A hook consists a property list with the following keys:

*   :update A function with no arguments that is called after the rack has updated its state.
*   :shutdown A function with no arguments that is called when the rack is shutting down.

Hooks must not modify the rack.

### Modules

**cl-synthesizer-modules-vco:vco-base** name environment transfer-function &key f-max v-peak (duty-cycle 0.5)

Creates a Voltage Controlled Oscillator module. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   transfer-function A function that converts the frequency control voltage into a frequency. This function is called with the current frequency control voltage and must return a frequency. Frequencies greater than f-max will be clipped. Negative frequencies will be clipped to 0Hz.
*   :f-max The maximum frequency of the oscillator. f-max must be greater than 0.
*   :v-peak Absolute value of the maximal voltage (positive/negative) emitted by the oscillator.
*   :duty-cycle The duty cycle of the square wave. 0 >= duty-cycle <= 1.

The module has the following inputs:

*   :cv Frequency control voltage.

The module has the following outputs:

*   :sine A sine wave.
*   :triangle A triangle wave.
*   :saw A saw wave.
*   :square A square wave.

See also modules vco-linear and vco-exponential.

* * *

**cl-synthesizer-modules-vco:vco-linear** name environment &key base-frequency f-max v-peak cv-max (duty-cycle 0.5)

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

The module has the following outputs: See vco-base.

* * *

**cl-synthesizer-modules-vco:vco-exponential** name environment &key base-frequency f-max v-peak (duty-cycle 0.5)

Creates a Voltage Controlled Oscillator module with 1V/Octave characteristic. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.
*   :f-max See vco-base.
*   :v-peak See vco-base.
*   :duty-cycle See vco-base.

The module has the following inputs:

*   :cv Frequency control voltage. For a given base-frequency of 440Hz a control voltage of 1.0 results in a frequency of 880Hz and a control voltage of -1.0 results in a frequency of 220Hz.

The module has the following outputs: See vco-base.

* * *

**cl-synthesizer-modules-vca:vca** name environment &key cv-max (initial-gain 0.0)

Creates a Voltage Controlled Amplifier/Attenuator module. The VCA multiplies an incoming signal with a factor of 0..1. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :cv-max The value of the effective amplification control voltage that represents the maximum amplification of 1.0.
*   :initial-gain An offset that is added to the amplification control voltage.

The module has the following inputs:

*   :cv Amplification control voltage.
*   :input Input signal to be amplified. The amplitude of this voltage is unknown to the VCA. It can have any value.
*   :gain An offset that is added to the amplification control voltage.

The effective amplification voltage is v = :cv + :gain + :initial-gain, where 0.0 <= v <= :cv-max. The module has the following outputs:

*   :output-linear Amplified input signal with linear amplification characteristic.
*   :output-exponential Amplified input signal with exponential amplification characteristic.

Examples can be found under /src/modules/vca/

* * *

**cl-synthesizer-modules-multiple:multiple** name environment &key output-count

Creates a Multiple module. A multiple mirrors one input to n outputs. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :output-count The number of outputs.

The module has the following inputs:

*   :input The input signal to be mirrored to the outputs.

The module has outputs :output-1 ... :output-n.

* * *

**cl-synthesizer-modules-midi-sequencer:midi-sequencer** name environment &key events

Creates a Midi-Sequencer module. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :events A list of Midi event declarations. Each declaration entry is a property list with the following keys:
    *   :timestamp-milli-seconds Point of time when events are to be fired.
    *   :midi-events List of Midi events to be fired. For the format of a single Midi event see /src/midi/event.lisp.

The module has no inputs. The module has one output socket :midi-events.

* * *

**cl-synthesizer-modules-midi-interface:midi-interface** name environment &key (voice-count 1) (channel nil) (note-number-to-cv (lambda (note-number) (/ note-number 12))) (play-mode :play-mode-poly) (cv-gate-on 5.0) (cv-gate-off 0.0) (controllers nil) (force-gate-retrigger nil)

Creates a MIDI interface module. The module dispatches MIDI-Note events to so called voices where each voice is represented by a control-voltage and a gate signal. The module supports the mapping of MIDI CC-Events to arbitary output sockets. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :voice-count The number of voices to be exposed by the module. Each voice consists of the following output sockets:
    *   :gate-n The gate signal. n = 1..voice-count.
    *   :cv-n A control voltage representing the note number. n = 1..voice-count
*   :channel Optional MIDI channel to which note events must belong. By default the channel is ignored. This setting does not effect the evaluation of CC-Events that are handled by controllers. Controllers must implement channel filtering on their own.
*   :note-number-to-cv An optional function that is called with a MIDI note number and returns a control-voltage.
*   :play-mode
    *   :play-mode-poly Polyphonic play mode. Incoming note events will be dispatched to "available" voices, where a voice is available when it meets certain criteria. These criteria are defined and implemented by the cl-synthesizer-midi-voice-manager:voice-manager package.
    *   :play-mode-unisono Monophonic play mode. All voices exposed by the module are set to the current "active" note. Notes are stacked. When a note is released, the voice outputs switch to the previous note. This logic is also implemented by the cl-synthesizer-midi-voice-manager:voice-manager package.
*   :cv-gate-on The "Gate on" control voltage.
*   :cv-gate-off The "Gate off" control voltage.
*   :force-gate-retrigger If t then in :play-mode-unisono play mode each note event will cause a retriggering of the gate signal. Otherwise the gate signal will stay on when it is already on.
*   :controllers Controllers can be used to declare additional output sockets that are exposed by the module. The controllers argument consists of a list of property lists with the following keys:
    *   :socket A keyword that defines the output socket to be exposed by the modules.
    *   :handler A property list that defines the keys
        *   :update A function that is called with the MIDI events passed to the update function of the module.
        *   :get-output A function with no arguments that returns the current value of the controller.For typical use cases refer to cl-synthesizer-midi:relative-cc-handler

Gate transitions are implemented as follows:

*   In :play-mode-poly play mode each incoming note causes that the gate signal of the assigned voice switches to On. If the gate signal of the assigned voice is already On (this happens when the available voices are exhausted and a voice is "stolen") then the gate signal switches to Off for the duration of one system tick and then to On again.
*   In :play-mode-unisono play mode incoming notes are stacked. The first note causes the gate signal to switch to On. Further "nested" note-on events only result in a change of the CV output but the gate signal will stay On. This behaviour can be overridden with the :force-gate-retrigger parameter.

The module has the following inputs:

*   :midi-events A list of MIDI events.

The module has the following outputs:

*   :gate-1 ... :gate-n
*   :cv-1 ... :cv-n
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
        

* * *

**cl-synthesizer-modules-envelope:envelope** name environment &key segments (gate-threshold 4.9)

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
*   :gate-threshold An optional threshold which defines the minimum input value of the :gate input that is interpreted as gate on.

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
        

* * *

**cl-synthesizer-modules-fixed-output:fixed-output** name environment &key value (output-socket :out)

Creates a module with a fixed output value. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :value The value of the module output.
*   :output-socket Optional keyword that declares the output socket identifier to be exposed by the module.

The module has no inputs. The module has one output socket according to the :output-socket argument.

### MIDI

#### MIDI Event

**cl-synthesizer-midi-event:make-control-change-event** channel controller-number value

Creates a MIDI control change event.

* * *

**cl-synthesizer-midi-event:make-note-on-event** channel note-number velocity

Creates a MIDI Note-On event.

* * *

**cl-synthesizer-midi-event:make-note-off-event** channel note-number velocity

Creates a MIDI Note-Off event.

* * *

**cl-synthesizer-midi-event:control-change-eventp** event

Returns t if the given MIDI event is a Control-Change event.

* * *

**cl-synthesizer-midi-event:note-on-eventp** event

Returns t if the given MIDI event is a Note-On event.

* * *

**cl-synthesizer-midi-event:note-off-eventp** event

Returns t if the given MIDI event is a Note-Off event.

* * *

**cl-synthesizer-midi-event:get-channel** event

Returns the MIDI channel number to which the event belongs.

* * *

**cl-synthesizer-midi-event:get-controller-number** event

Returns the controller number of the MIDI event.

* * *

**cl-synthesizer-midi-event:get-controller-value** event

Returns the controller value of the MIDI event.

* * *

**cl-synthesizer-midi-event:get-note-number** event

Returns the note number of the MIDI event.

* * *

**cl-synthesizer-midi-event:get-velocity** event

Returns the velocity of the MIDI event.

#### MIDI Utilities

**cl-synthesizer-midi:get-note-number-frequency** note-number

Returns the frequency of a given note number. Note number 69 results in a frequency of 440Hz. This function implements a simple mapping and might be useful in some cases. For more details about the implementation refer to the source code.

* * *

**cl-synthesizer-midi:relative-cc-handler** midi-controller controllers &key cv-initial cv-min cv-max (channel nil)

Creates a handler that maps MIDI events of n >= 1 relative MIDI CC-Controllers to a single target value. The function has the following arguments:

*   midi-controller A property list with the keys
    *   :get-controller-number A function with one argument that is called with a keyword that identifies the controller, for example :encoder-1 and returns the controller number, for example 112.
    *   :get-controller-value-offset A function with one argument that is called with the value of a relative CC event, for example 62, and returns a positive or negative offset, for example -3.
*   controllers A list of property lists with the keys
    *   :controller-id A keyword that identifies an encoder of the given midi-controller, for example :encoder-1
    *   :weight The weight of the controller in percent that defines how much the target value will be increased/decreased when the controller is turned. The value is relative to the total control voltage range as defined by cv-min and cv-max.
    *   :turn-speed An optional function that is called with the absolute value of the increase/decrease offset as returned by the :get-controller-value-offset function of the midi-controller. The offset typically depends on the speed with which the encoder is turned. The function must return the absolute value of the new offset. This function can for example be used to disable turn-speed specific increments/decrements by simply returning 1. Example: (:turn-speed (lambda (offs) 1))
*   :cv-initial The initial output value of the handler function.
*   :cv-min The minimum output value of the handler function. Clipping is applied to ensure this.
*   :cv-max The maximum output value of the handler function. Clipping is applied to ensure this.
*   :channel Optional number of the MIDI channel to which the controller events must belong. By default the channel number is ignored.

The returned handler is a property list with the following keys:

*   :update A function that is to be called with a list of midi-events.
*   :get-output A function that returns the current output value.

Examples: see modules/midi-interface/

### Monitor

**cl-synthesizer-monitor:add-monitor** rack monitor-handler socket-mappings &rest additional-handler-args

Adds a monitor to a rack. A monitor is a high-level Rack hook that collects module states (input/output sockets) and passes them to a monitor handler. A monitor handler can for example be a Wave-File-Writer. The function has the following arguments:

*   rack The rack.
*   monitor-handler A function that instantiates the monitor handler. This function is called with the following arguments:
    
    *   name A name.
    *   environment The synthesizer environment.
    *   output-keywords A list of keywords declaring the keyword parameters with which the monitor handler update function will be called.
    *   additional-handler-args Any additional keyword parameters as passed to the monitor function. These parameters can be used to initialize handler specific properties such as a filename.
    
    The function must return a property list with the following keys:
    *   :update A function that is called after each tick of the rack. It is called with keyword parameters as declared by the output-keywords argument described above.
    *   :shutdown An optional function with no arguments that is called when the rack shuts down.
*   socket-mappings Declares a list of mappings of specific sockets of specific modules to keyword parameters that will be passed to the update function of the handler. Each mapping entry has the following format:
    *   key Keyword to be used as keyword parameter when calling the update function of the handler, for example :channel-1. For now this key must be one that is supported by the actual handler. For example the Wave-File handler only supports :channel-n keys.
    *   module-name Name of the module from which the state of a certain input/output socket is to be retrieved, for example "ADSR"
    *   socket-type Defines if the value of an input-socket is requested or the value of an output-socket. Must be :input-socket or :output-socket
    *   socket A keyword that identifies one of the input/output sockets provided by the module, for example :cv
*   &rest additional-handler-args Optional keyword arguments to be passed to the handler instantiation function.

Example:

    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "LFO" :output-socket :saw)
           (:channel-2 "ADSR" :output-socket :cv)
           (:channel-3 "LINE-OUT" :input-socket :channel-1))
         :filename "trace.wav")
        

* * *

**cl-synthesizer-monitor-wave-handler:wave-file-handler** name environment outputs &rest rest &key filename &allow-other-keys

Creates a monitor handler which writes its inputs into a Wave file. The function has the following arguments:

*   name A name.
*   environment The synthesizer environment.
*   outputs The output keys as defined by the Monitor Socket-Mapping. For now these must be :channel-1 ... :channel-n.
*   :filename A file path relative to the output directory as defined by the environment.

### Device

**cl-synthesizer:make-device** name environment device-settings

Creates a device. The function has the following arguments:

*   name Name of the device.
*   environment The synthesizer environment.
*   device-settings The device settings consist of a property list with the following keys:
    *   :symbol-name Symbol name of the device instantiation function, for example "SPEAKER-CL-OUT123"
    *   :package-name Package name of the device instantiation function, for example "CL-SYNTHESIZER-DEVICE-SPEAKER"
    *   :init-args A list of additional arguments to be passed to the device instantiation function. An argument may consist of a function. In this case the value of the argument to be passed to the device instantiation function will be evaluated by calling the given function with the environment as parameter. Example: `(:channel-count (lambda (environment) (getf environment :channel-count)) :driver "coreaudio"))`

The device instantiation function is called with the following arguments:

*   name Name of the device.
*   environment The synthesizer environment.
*   Any additional arguments as declared by :init-args

If the device represents a MIDI input device then the device instantiation function must return a property list with the following keys (keys are not finalized yet by implementation):

*   :inputs nil
*   :outputs (:midi-events).
*   :get-output A function that returns the current output of the underlying device as a list of Midi-Events.
*   :shutdown An optional shutdown function that is called when the rack is shutting down.

If the device represents an Audio output device then the device instantiation function must return a property list with the following keys (keys are not finalized yet by implementation):

*   :inputs (:channel-1 ... :channel-n) Where n is the number of output channels as declared by the environment.
*   :outputs nil.
*   :update Function that is called with keywords parameters :channel-1 ... :channel-n in order to push audio data to the underlying device.
*   :shutdown An optional shutdown function that is called when the rack is shutting down.

* * *

**cl-synthesizer-device-speaker:speaker-cl-out123** name environment &key channel-count driver (buf-length-frames 1000) (v-peak 5.0)

Creates a speaker device. The device is using the cl-out123 package to push audio data to a system speaker driver. The :update function as exposed by the device is blocking. This means that when the maximum buffer size has been reached, the function will not return until the speaker driver has accepted the buffer. This behaviour can be used to synchronize the synthesizer. The device has a latency of about 300-400ms and therefore cannot really be used for real-time play using a Midi-Controller. The function has the following arguments:

*   name A name.
*   environment The synthesizer environment.
*   :channel-count Number of output channels.
*   :driver Driver to be used, for example "coreaudio".
*   :v-peak Optional peak voltage. The inputs of the device will be normalized to -1.0 ... 1.0 according to v-peak. Incoming voltages will not be clipped.
*   :buf-length-frames Number of frames to be buffered until the audio data is pushed to the driver.

The device has the following inputs:

*   :channel-1 ... :channel-n In a stereo setup left is represented by :channel-1 and right by :channel-2

The module has no outputs. The current buffer is flushed when the :shutdown function as exposed by the device is being called.

* * *

**cl-synthesizer-device-midi:midi-device** name environment

* * *

Generated 2018-09-27 22:32:04