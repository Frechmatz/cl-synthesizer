cl-synthesizer
==============

A Modular Audio Synthesizer library implemented in Common Lisp.

**Example:**

    (defpackage :cl-synthesizer-rack-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-rack-example-1)
    
    (defun example ()
      "Two frequency modulated saw signals on left and right channel."
      (flet ((make-saw-signal (name environment &key lfo-frequency vco-frequency)
    	   "Creates a module which generates a frequency modulated saw signal."
    	   (declare (ignore name))
    	   (let ((voice
    		  (cl-synthesizer:make-rack
    		   :environment environment
    		   ;; Expose audio output socket
    		   :output-sockets '(:audio))))
    
    	     ;; Add LFO
    	     (cl-synthesizer:add-module
    	      voice "LFO"
    	      #'cl-synthesizer-modules-vco:make-linear-module
    	      :base-frequency lfo-frequency :v-peak 1.0 :f-max 500 :cv-max 5)
    
    	     ;; Add VCO
    	     (cl-synthesizer:add-module
    	      voice "VCO"
    	      #'cl-synthesizer-modules-vco:make-linear-module
    	      :base-frequency vco-frequency :f-max 5000 :v-peak 5 :cv-max 5)
    
    	     ;; Patch LFO with VCO
    	     (cl-synthesizer:add-patch voice "LFO" :sine "VCO" :cv)
    
    	     ;; Patch VCO with audio output of module
    	     ;; OUTPUT is a virtual module that represents the output sockets of the rack.
    	     (cl-synthesizer:add-patch voice "VCO" :saw "OUTPUT" :audio)
    	     
    	     voice)))
    
        ;; Set up the synthesizer
        (let ((rack (cl-synthesizer:make-rack
    		 :environment (cl-synthesizer:make-environment)
    		 ;; Expose left and right channel line-out sockets
    		 :output-sockets '(:left :right))))
    
          ;; Add saw-signal generators
          (cl-synthesizer:add-module
           rack "VOICE-1" #'make-saw-signal :lfo-frequency 1.0 :vco-frequency 440)
          (cl-synthesizer:add-module
           rack "VOICE-2" #'make-saw-signal :lfo-frequency 2.0 :vco-frequency 442)
    
          ;; Patch generators with left/right outputs
          (cl-synthesizer:add-patch rack "VOICE-1" :audio "OUTPUT" :left)
          (cl-synthesizer:add-patch rack "VOICE-2" :audio "OUTPUT" :right)
    
          ;; Write outputs to a Wave-File
          (cl-synthesizer-monitor:add-monitor
           rack
           #'cl-synthesizer-monitor-wave-handler:wave-file-handler
           '((:channel-1 "OUTPUT" :input-socket :left)
             (:channel-2 "OUTPUT" :input-socket :right))
           :filename "rack-example-1.wav")
          
          rack)))
    
    (defparameter *attach-audio* nil)
    #|
    ;; Play rack for five seconds.
    (cl-synthesizer:play-rack (example) 5 
        :attach-audio *attach-audio* :audio-output-sockets '(:left :right))
    |#

Installation
------------

    
    cd ~/quicklisp/local-projects
    git clone https://github.com/Frechmatz/cl-wave.git
    git clone https://github.com/Frechmatz/cl-synthesizer.git
    (ql:quickload "cl-synthesizer")
    

The cl-wave repository is a fork of the wave file reader/writer library implemented by [Ryan King](https://github.com/RyanTKing/cl-wave) where a dependency has been replaced with code kindly provided by Ryan.

MIDI and Audio support for MacOS:

    
    cd ~/quicklisp/local-projects
    git clone https://github.com/byulparan/CoreMIDI.git
    ln -s cl-synthesizer cl-synthesizer-macos-device
    (ql:quickload "cl-synthesizer-macos-device")
    

Installs the devices **cl-synthesizer-device-midi:midi-device** and **cl-synthesizer-device-speaker:speaker-cl-out123**.

API Reference
-------------

*   [Environment](#environment)
*   [Rack](#rack)
*   [Modules](#modules)
    *   [VCO](#vco)
    *   [VCA](#vca)
    *   [Envelope](#envelope)
    *   [Multiple](#multiple)
    *   [MIDI Interface](#midi-interface)
    *   [MIDI CC Interface](#midi-cc-interface)
    *   [MIDI Sequencer](#midi-sequencer)
    *   [Fixed Output](#fixed-output)
    *   [Adder](#adder)
    *   [Mixer](#mixer)
*   [Monitor](#monitor)
*   [MIDI](#midi)
    *   [MIDI Event](#midi-event)
    *   [MIDI Utilities](#midi-utilities)
*   [Conditions](#conditions)

### Environment

**cl-synthesizer:make-environment** &key (sample-rate 44100) (home-directory nil)

Creates an environment. The environment defines properties such as the sample rate of the rack. An enviroment is a property list with the following keys:

*   :sample-rate Sample rate of the synthesizer.
*   :home-directory The base output directory for wave files etc. Default value is the home directory of the current user.

### Rack

**cl-synthesizer:make-rack** &key environment (input-sockets nil) (output-sockets nil)

Creates a rack. A rack is a module container and also a module, which means that racks can be added to other racks. The function has the following arguments:

*   :environment The synthesizer environment.
*   :input-sockets The input sockets to be exposed by the rack. The inputs can be accessed for patching of inner modules of the rack via the virtual module "INPUT".
*   :output-sockets The output sockets to be exposed by the rack. The outputs can be accessed for patching of inner modules of the rack via the virtual module "OUTPUT".

The update function of the rack calls the update function of all modules that have been added to the rack. If the rack has already been shut down it immediately returns **nil**. Othwerwise it returns **t**.

The shutdown function shuts the rack down by calling the shutdown handlers of all modules and hooks of the rack. If the rack has already been shut down the function does not call any handlers.

See also: add-module

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
    
    *   :inputs A function with no arguments that returns a list of keywords that represent the input sockets exposed by the module.
    *   :outputs A function with no arguments that returns a list of keywords that represent the output sockets exposed by the module.
    *   :update A function that is called with the values of the modules input sockets in order to update the state of the module (the state of its output sockets). The value of each input socket is passed via a keyword parameter.
    *   :get-output A function that is called in order to get the value of a specific output socket. The function is called with a keyword that identifies the output socket whose state is to be returned. The function must not modify the value of the given or any other output socket.
    *   :shutdown An optional function with no arguments that is called when the rack is shutting down.
    
    The input/output socket lists exposed by the module are not buffered by the rack. Therefore the module should return either a quoted list or keep it in an internal variable. The module must not add or remove input/output sockets after it has been instantiated.
    
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

**cl-synthesizer:play-rack** rack duration-seconds &key (attach-audio nil) (audio-output-sockets nil) (attach-midi nil) (midi-input-socket nil)

A utility function that "plays" the rack by consecutively calling its update function for a given number of "ticks". The function has the following arguments:

*   rack The rack.
*   duration-seconds Duration in seconds of how long to play the rack. If for example the duration is 2 seconds and the sample rate of the rack as declared by its environment is 44100, then the update function of the rack will be called 88200 times.
*   :attach-audio If t then the audio device as declared by the variable \*audio-device-settings\* is instantiated and attached to the given outputs of the rack.
*   :audio-output-sockets A list of keywords that declare the output sockets of the rack providing the audio signal.
*   :attach-midi If t then the MIDI device as declared by the variable \*midi-device-settings\* is instantiated and attached to the rack.
*   :midi-input-socket A keyword that declares the input socket of the rack to which the MIDI input is to be routed.

The current implementation of the play-rack function assumes that an audio device is blocking.

See also: cl-synthesizer-device-speaker:speaker-cl-out123, cl-synthesizer-device-midi:midi-device

* * *

**cl-synthesizer:get-environment** rack

Returns the environment of the rack.

* * *

**cl-synthesizer:get-module** rack name

Get a module of a rack. The function has the following arguments:

*   rack The rack.
*   name The name of the module

Returns the module (represented as a property list) or nil if a module with the given name has not been added to the rack.

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

**cl-synthesizer:add-hook** rack hook

Adds a hook to the rack. A hook is called each time after the rack has updated its state. A hook consists a property list with the following keys:

*   :update A function with no arguments that is called after the rack has updated its state.
*   :shutdown A function with no arguments that is called when the rack is shutting down.

Hooks must not modify the rack. See also **cl-synthesizer-monitor:add-monitor**.

### Modules

#### VCO

**cl-synthesizer-modules-vco:make-module-base** name environment transfer-function &key f-max v-peak (duty-cycle 0.5)

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

**cl-synthesizer-modules-vco:make-linear-module** name environment &key base-frequency f-max v-peak cv-max (duty-cycle 0.5)

Creates a Voltage Controlled Oscillator module with linear characteristic. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :cv-max The frequency control voltage which represents the maximum frequency of the oscillator.
*   :base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.
*   :f-max See make-module-base.
*   :v-peak See make-module-base.
*   :duty-cycle See make-module-base.

The module has the following inputs:

*   :cv Frequency control voltage. For frequency calculation the absolute value of the control voltage is used. The control voltage is clipped at :cv-max.

For the output sockets of the module see make-module-base.

**Example:**

    (defpackage :cl-synthesizer-modules-vco-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-vco-example-1)
    
    (defun example ()
      "Write all wave forms into a 4-Channel wave file"
      (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
        (cl-synthesizer:add-module
         rack
         "VCO"
         #'cl-synthesizer-modules-vco:make-linear-module :base-frequency 10 :v-peak 5 :cv-max 5 :f-max 12000)
        
        ;; Record outputs into a Wave-File
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "VCO" :output-socket :sine)
           (:channel-2 "VCO" :output-socket :triangle)
           (:channel-3 "VCO" :output-socket :saw)
           (:channel-4 "VCO" :output-socket :square))
         :filename "waves/vco-example-1.wav")
    
        rack))
          
    ;;(cl-synthesizer:play-rack (example) 3)

**cl-synthesizer-modules-vco:make-exponential-module** name environment &key base-frequency f-max v-peak (duty-cycle 0.5)

Creates a Voltage Controlled Oscillator module with 1V/Octave characteristic. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :base-frequency The frequency emitted by the oscillator at a frequency control voltage of 0.
*   :f-max See make-module-base.
*   :v-peak See make-module-base.
*   :duty-cycle See make-module-base.

The module has the following inputs:

*   :cv Frequency control voltage. For a given base-frequency of 440Hz a control voltage of 1.0 results in a frequency of 880Hz and a control voltage of -1.0 results in a frequency of 220Hz.

For the output sockets of the module see make-module-base.

* * *

#### VCA

**cl-synthesizer-modules-vca:make-module** name environment &key cv-max (initial-gain 0.0)

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

**Example:**

    (defpackage :cl-synthesizer-modules-vca-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-vca-example-1)
    
    (defun example ()
      "Amplification of a 10kHz sine wave with a bipolar triangular signal."
      (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
        ;; Set up oscillator modulating the amplification
        (cl-synthesizer:add-module
         rack "LFO-CV"
         #'cl-synthesizer-modules-vco:make-linear-module
         :base-frequency 0.5
         :v-peak 5.0
         :cv-max 5.0
         :f-max 12000)
    
        ;; set up oscillator providing the audio signal
        (cl-synthesizer:add-module
         rack "VCO-AUDIO"
         #'cl-synthesizer-modules-vco:make-linear-module
         :base-frequency 10000.0
         :v-peak 5.0
         :cv-max 5.0
         :f-max 12000)
    
        ;; Set up VCA
        (cl-synthesizer:add-module
         rack "VCA"
         #'cl-synthesizer-modules-vca:make-module
         :cv-max 5.0)
    
        ;; Add patches
        (cl-synthesizer:add-patch rack "VCO-AUDIO" :sine "VCA" :input)
        (cl-synthesizer:add-patch rack "LFO-CV" :triangle "VCA" :cv)
    
        ;; Record VCA inputs/outputs into a Wave-File
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "VCA" :input-socket :cv)
           (:channel-2 "VCA" :input-socket :input)
           (:channel-3 "VCA" :output-socket :output-linear)
           (:channel-4 "VCA" :output-socket :output-exponential))
         :filename "waves/vca-example-1.wav")
    
        rack))
    
    ;;(cl-synthesizer:play-rack (example) 5)

#### Envelope

**cl-synthesizer-modules-envelope:make-module** name environment &key segments (gate-threshold 4.9)

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
    *   :output-max The maximum target value of the mapping.
    
    Clipping is generally not applied except for cases such as a negative segment duration. Controller inputs are always offsets that are added to the initial value as provided by :duration-ms or :target-cv. Controller inputs do not affect the behaviour of the currently active segment.
    
*   :gate-threshold An optional threshold which defines the minimum input value of the :gate input that is interpreted as gate on.

The module has the following inputs:

*   :gate The gate signal as provided for example by a MIDI sequencer. If the gate switches from :off to :on the output voltage is reset to 0.0 and the module switches to the first segment.
*   Inputs as defined by segment controllers.

The module has the following outputs:

*   :cv The current value of the envelope. The initial value is 0.0

**Example:**

    (defpackage :cl-synthesizer-modules-envelope-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-envelope-example-1)
    
    (defun example ()
      "Simple envelope example"
      (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
    
        (cl-synthesizer:add-module
         rack "ADSR"
         #'cl-synthesizer-modules-envelope:make-module
         :segments
         '(;; Attack (duration can be modulated via input socket :attack-duration)
           (:duration-ms 100 :target-cv 5 :required-gate-state :on
            :duration-controller
            (:socket :attack-duration :input-min 0.0 :input-max 5.0 :output-min 0 :output-max 800))
           ;; Decay
           (:duration-ms 50 :target-cv 3 :required-gate-state :on)
           ;; Sustain
           (:required-gate-state :on)
           ;; Release
           (:duration-ms 100 :target-cv 0 :required-gate-state :off)))
    
        rack))

#### Multiple

**cl-synthesizer-modules-multiple:make-module** name environment &key output-count

Creates a Multiple module. A multiple passes the value of exactly one input socket to as many output sockets as defined by output-count. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :output-count The number of output sockets.

The module has the following inputs:

*   :input The input signal to be passed to the outputs.

The module has the following outputs:

*   :output-1 ... :output-n. Where n is the output-count.

**Example:**

    (defpackage :cl-synthesizer-modules-multiple-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-multiple-example-1)
    
    (defun example ()
      "Multiple example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       :output-sockets '(:line-out-1 :line-out-2))))
        
        (cl-synthesizer:add-module
         rack "LFO"
         #'cl-synthesizer-modules-vco:make-linear-module
         :base-frequency 1.0 :v-peak 1.0 :f-max 500 :cv-max 5)
        
        (cl-synthesizer:add-module rack "MULTIPLE"
    			       #'cl-synthesizer-modules-multiple:make-module :output-count 5)
        (cl-synthesizer:add-patch rack "LFO" :sine "MULTIPLE" :input)
        (cl-synthesizer:add-patch rack "MULTIPLE" :output-1 "OUTPUT" :line-out-1)
        (cl-synthesizer:add-patch rack "MULTIPLE" :output-2 "OUTPUT" :line-out-2)
    
        rack))
    
    ;;(cl-synthesizer:play-rack (example) 1)

#### MIDI Interface

**cl-synthesizer-modules-midi-interface:make-module** name environment &key (voice-count 1) (channel nil) (note-number-to-cv (lambda (note-number) (/ note-number 12))) (play-mode :play-mode-poly) (cv-gate-on 5.0) (cv-gate-off 0.0) (force-gate-retrigger nil)

Creates a MIDI interface module. The module dispatches MIDI-Note events to so called voices where each voice is represented by a control-voltage and a gate signal. The module supports the mapping of MIDI CC-Events to arbitary output sockets. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :voice-count The number of voices to be exposed by the module. Each voice consists of the following output sockets:
    *   :gate-n The gate signal. n = 1..voice-count.
    *   :cv-n A control voltage representing the note number. n = 1..voice-count.
*   :channel Optional MIDI channel to which note events (not CC-Events) must belong. By default the channel is ignored.
*   :note-number-to-cv An optional function that is called with a MIDI note number and returns a control-voltage.
*   :play-mode
    
    *   :play-mode-poly Polyphonic play mode. Incoming note events will be dispatched to "available" voices.
    *   :play-mode-unisono Monophonic play mode. All voices exposed by the module are set to the current "active" note. Notes are stacked. When a note is released, the voice outputs switch to the previous note.
    
    The handling of play-modes is implemented by the package cl-synthesizer-midi-voice-manager:voice-manager.
    
*   :cv-gate-on The "Gate on" control voltage.
*   :cv-gate-off The "Gate off" control voltage.
*   :force-gate-retrigger If t then in :play-mode-unisono play mode each note event will cause a retriggering of the gate signal. Otherwise the gate signal will just stay on when it is already on.

Gate transitions are implemented as follows:

*   In :play-mode-poly play mode each incoming note causes that the gate signal of the assigned voice switches to On. If the gate signal of the assigned voice is already On (this happens when the available voices are exhausted and a voice is "stolen") then the gate signal switches to Off for the duration of one system tick and then to On again.
*   In :play-mode-unisono play mode incoming notes are stacked. The first note causes the gate signal to switch to On. Further "nested" note-on events only result in a change of the CV output but the gate signal will stay On. This behaviour can be overridden with the :force-gate-retrigger parameter.

The module has the following inputs:

*   :midi-events A list of MIDI events.

The module has the following outputs:

*   :gate-1 ... :gate-n
*   :cv-1 ... :cv-n

**Example:**

    (defpackage :cl-synthesizer-modules-midi-interface-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-midi-interface-example-1)
    
    (defparameter *attach-midi* t)
    (defparameter *attach-audio* t)
    
    (defun example ()
      "Very simple midi-interface example that does not care about the gate signal."
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       :input-sockets '(:midi-events)
    	       :output-sockets '(:line-out))))
    
        (cl-synthesizer:add-module
         rack "MIDI-IFC" #'cl-synthesizer-modules-midi-interface:make-module
         :voice-count 1)
    
        (cl-synthesizer:add-module
         rack "VCO-1"
         #'cl-synthesizer-modules-vco:make-exponential-module
         :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
         :f-max 13000
         :v-peak 5)
        
        (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-IFC" :midi-events)
        (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO-1" :cv)
        (cl-synthesizer:add-patch rack "VCO-1" :saw "OUTPUT" :line-out)
        rack))
    
    #|
    (cl-synthesizer::play-rack (example) 10 
        :attach-audio t :audio-output-sockets '(:line-out) 
        :attach-midi t :midi-input-socket :midi-events)
    |#

#### MIDI CC Interface

**cl-synthesizer-modules-midi-cc-interface:make-module** name environment &key controller-numbers transform-handler (channel nil) (initial-output 0) (min-output nil) (max-output nil)

Creates a MIDI CC Event interface module. The module maps MIDI control change events to an output value. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :controller-numbers A list of MIDI controller numbers.
*   :transform-handler A function that converts a control value to the output value of the module. It is called for each matching CC event and has the following arguments:
    *   The current output value of the module.
    *   Controller number.
    *   Control value.The function must return the new output value of the module.
*   :channel Optional number of the MIDI channel to which the controller events must belong. By default there is no channel filtering applied.
*   :initial-output The initial output value of the module.
*   :min-output Optional lowest numeric output value of the module. If the transform handler returns a number smaller than min-output then the actual output-value is set to min-output.
*   :max-output Optional largest numeric output value of the module. If the transform handler returns a number greater than max-output then the actual output value is set to max-output.

The module has the following inputs:

*   :midi-events A list of MIDI events.

The module has the following outputs:

*   :output The current output value.

**Example:**

    (defpackage :cl-synthesizer-modules-midi-cc-interface-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-midi-cc-interface-example-1)
    
    (defun example ()
      "MIDI CC-Interface Example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       :input-sockets '(:midi-events))))
    
        (cl-synthesizer:add-module
         rack "MIDI-CC-IFC" #'cl-synthesizer-modules-midi-cc-interface:make-module
         :controller-numbers '(112)
         :initial-output 2.5
         :min-output 0.0
         :max-output 5.0
         :transform-handler
         (lambda (cur-output controller-number control-value)
           (declare (ignore controller-number))
           (cond
    	 ((= control-value 61)
    	  (+ cur-output -0.5))
    	 ((= control-value 67)
    	  (+ cur-output 0.5))
    	 (t cur-output)))
         :channel nil)
        
        (cl-synthesizer:add-patch rack "INPUT" :midi-events "MIDI-CC-IFC" :midi-events)
    
        rack))
    
    #|
    (cl-synthesizer::play-rack (example) 5)
    |#

#### MIDI Sequencer

**cl-synthesizer-modules-midi-sequencer:make-module** name environment &key events

Creates a Midi-Sequencer module. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :events A list of Midi events and their timestamps. Each entry consists of a property list with the following keys:
    *   :timestamp-milli-seconds Point of time when events are to be fired. The very first timestamp of the synthesizer is 0.
    *   :midi-events List of Midi events to be fired.

The module has no inputs. The module has one output socket :midi-events.

**Example:**

    (defpackage :cl-synthesizer-modules-midi-sequencer-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-midi-sequencer-example-1)
    
    (defparameter *attach-audio* t)
    
    (defun example ()
      "Midi-Sequencer example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       :output-sockets '(:line-out))))
    
        ;; Add sequencer
        (cl-synthesizer:add-module
         rack
         "MIDI-SEQUENCER"
         #'cl-synthesizer-modules-midi-sequencer:make-module :events
         (list 
          (list :timestamp-milli-seconds 0
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
          (list :timestamp-milli-seconds 1000
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
          (list :timestamp-milli-seconds 2000
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-on-event 1 75 100)))
          (list :timestamp-milli-seconds 2500
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-off-event 1 75 100)))))
    
        ;; Add MIDI Interface and connect it with the MIDI Sequencer
        (cl-synthesizer:add-module
         rack
         "MIDI-IFC"
         #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)
        (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
    
        ;; Add VCO
        (cl-synthesizer:add-module
         rack "VCO" #'cl-synthesizer-modules-vco:make-exponential-module
         :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
         :f-max 12000
         :v-peak 5)
    
        ;; Add ADSR
        (cl-synthesizer:add-module
         rack "ADSR"
         #'cl-synthesizer-modules-envelope:make-module
         :segments
         '(;; Attack
           (:duration-ms 100 :target-cv 5 :required-gate-state :on)
           ;; Decay
           (:duration-ms 50 :target-cv 3 :required-gate-state :on)
           ;; Sustain
           (:required-gate-state :on)
           ;; Release
           (:duration-ms 100 :target-cv 0 :required-gate-state :off)))
        
        ;; Add VCA
        (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:make-module :cv-max 5.0)
    
        ;; Connect VCA with ADSR and VCO
        (cl-synthesizer:add-patch rack "VCA" :output-linear "OUTPUT" :line-out)
        (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
        (cl-synthesizer:add-patch rack "VCO" :triangle "VCA" :input)
        
        ;; Connect Midi interface with ADSR and VCO
        (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv)
        (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    
        ;; Record LINE-OUT into a wave file
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "OUTPUT" :input-socket :line-out))
         :filename "waves/midi-sequencer-example-1.wav")
        
        rack))
    
    #|
    (cl-synthesizer::play-rack (example) 5 
        :attach-audio t :audio-output-sockets '(:line-out))
    |#

#### Fixed Output

**cl-synthesizer-modules-fixed-output:make-module** name environment &key value (output-socket :out)

Creates a module with a fixed output value. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :value The value of the module output.
*   :output-socket Optional keyword that declares the output socket identifier to be exposed by the module.

The module has no inputs. The module has one output socket according to the :output-socket argument.

**Example:**

    (defpackage :cl-synthesizer-modules-fixed-output-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-fixed-output-example-1)
    
    (defun example ()
      "Fixed-Output example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       :output-sockets '(:line-out))))
        
        (cl-synthesizer:add-module
         rack "FIXED-OUTPUT"
         #'cl-synthesizer-modules-fixed-output:make-module
         :value 3.0
         :output-socket :fixed)
        
        (cl-synthesizer:add-patch rack "FIXED-OUTPUT" :fixed "OUTPUT" :line-out)
    
        rack))
    
    ;;(cl-synthesizer:play-rack (example) 1)

#### Adder

**cl-synthesizer-modules-adder:make-module** name environment &key input-count

Creates a simple voltage adder module. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :input-count The number of input sockets.

The module has the following inputs:

*   :input-1 ... :input-n. Where n is the input-count. Input values not of type **number** are ignored.

The module has the following outputs:

*   :output The output consisting of the sum of the inputs.

#### Mixer

**cl-synthesizer-modules-mixer:make-module** name environment &key channel-count (channel-cv-max 5.0) (channel-cv-gain 5.0) (main-cv-max 5.0) (main-cv-gain 5.0)

Creates a mixer module. The mixer provides an attenuator for each input and a main attenuator for the mixer output. All attenuators have linear amplification characteristic. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :channel-count The number of channels.
*   :channel-cv-max The value of a channel attenuation control voltage that represents an amplification of 1.0.
*   :channel-cv-gain An offset that is added to the channel attenuation control voltage.
*   :main-cv-max The value of the main attenuation input that represents an amplification of 1.0.
*   :main-cv-gain An offset that is added to the main attenuation control voltage.

The module has the following inputs:

*   :channel-1 ... :channel-n. Channel input signal, where n is the channel-count.
*   :cv-1 ... :cv-n. Channel attenuation control voltage, where n is the channel-count.
*   :cv-main Attenuation control voltage of the mixer output.

The module has the following outputs:

*   :output The output consisting of the sum of the inputs.

**Example:**

    (defpackage :cl-synthesizer-modules-mixer-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-mixer-example-1)
    
    (defun example ()
      "Mixer example."
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       :output-sockets '(:line-out))))
    
        ;;
        ;; add modules...
        ;;
        
        (cl-synthesizer:add-module
         rack "MIXER" #'cl-synthesizer-modules-mixer:make-module
         :channel-count 2
         :channel-cv-max 5.0
         :channel-cv-gain 5.0
         :main-cv-max 5.0
         :main-cv-gain 2.5)
        
        (cl-synthesizer:add-patch rack "VOICE-1" :audio "MIXER" :channel-1)
        (cl-synthesizer:add-patch rack "VOICE-2" :audio "MIXER" :channel-2)
        (cl-synthesizer:add-patch rack "MIXER" :output "OUTPUT" :line-out)
        
        rack))

### Monitor

**cl-synthesizer-monitor:add-monitor** rack monitor-handler socket-mappings &rest additional-handler-args

Adds a monitor to a rack. A monitor is a high-level Rack hook that collects module states (values of input/output sockets) and passes them to a monitor handler. A monitor handler can for example be a Wave-File-Writer. The function has the following arguments:

*   rack The rack.
*   monitor-handler A function that instantiates the monitor handler. This function is called with the following arguments:
    
    *   name A name.
    *   environment The synthesizer environment.
    *   input-keywords A list of keywords declaring the keyword parameters with which the monitor handler update function will be called.
    *   additional-handler-args Any additional keyword parameters as passed to the monitor function. These parameters can be used to initialize handler specific properties such as a filename.
    
    The function must return a property list with the following keys:
    *   :update A function that is called after each tick of the rack. It is called with keyword parameters as declared by the input-keywords argument described above.
    *   :shutdown An optional function with no arguments that is called when the rack shuts down.
*   socket-mappings Declares a list of mappings of specific sockets of specific modules to keyword parameters that will be passed to the update function of the handler. Each mapping entry has the following format:
    *   key Keyword to be used as keyword input parameter when calling the update function of the handler, for example :channel-1. For now this key must be one that is supported by the actual handler. For example the Wave-File handler only supports input keys :channel-1 ... :channel-n.
    *   module-name Name of the module from which the state of a certain input/output socket is to be retrieved, for example "ADSR"
    *   socket-type Defines if the value of an input-socket is to be passed to the handler or the value of an output-socket. Must be :input-socket or :output-socket
    *   socket A keyword that identifies one of the input/output sockets provided by the module, for example :cv
*   &rest additional-handler-args Optional keyword arguments to be passed to the handler instantiation function.

**Example:**

    (defpackage :cl-synthesizer-monitor-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-monitor-example-1)
    
    (defun example ()
      "Monitor example."
      (let ((rack (cl-synthesizer:make-rack
    	       :environment
    	       (cl-synthesizer:make-environment))))
    
        ;;
        ;; add modules...
        ;;
        
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:wave-file-handler
         '((:channel-1 "ADSR" :input-socket :gate)
           (:channel-2 "ADSR" :output-socket :cv))
         :filename "monitor-example-1.wav")
        
        rack))

**cl-synthesizer-monitor-wave-handler:wave-file-handler** name environment inputs &rest rest &key filename &allow-other-keys

Creates a monitor handler which writes its inputs into a Wave file. The function has the following arguments:

*   name A name.
*   environment The synthesizer environment.
*   inputs The input keys as defined by the Monitor Socket-Mapping. For now these must be :channel-1 ... :channel-n.
*   :filename A file path relative to the output directory as defined by the environment.

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

Returns the controller number of a Control-Change MIDI event.

* * *

**cl-synthesizer-midi-event:get-controller-value** event

Returns the controller value of a Control-Change MIDI event.

* * *

**cl-synthesizer-midi-event:get-note-number** event

Returns the note number of Note-On/Off MIDI event.

* * *

**cl-synthesizer-midi-event:get-velocity** event

Returns the velocity of a Note-On/Off MIDI event.

#### MIDI Utilities

**cl-synthesizer-midi:get-note-number-frequency** note-number

Returns the frequency of a given note number. Note number 69 results in a frequency of 440Hz. This function implements a simple mapping and might be useful in some cases. For more details about the implementation refer to the source code.

### Conditions

**assembly-error**

This condition is signalled in cases where the assembly of a rack fails, because for example a module name is not unique, a patch is added for a non-existing module, a patch is added to an already patched socket and so on.

* * *

Generated 2018-11-12 20:43:50