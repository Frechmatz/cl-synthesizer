cl-synthesizer
==============

A Modular Audio Synthesizer library implemented in Common Lisp.

**Example:**

    (defpackage :cl-synthesizer-rack-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-rack-example-1)
    
    (defun make-voice (name environment &key lfo-frequency vco-frequency)
      "Frequency modulated saw"
      (declare (ignore name))
      (let ((voice
    	 (cl-synthesizer:make-rack
    	  :environment environment
    	  ;; Expose audio output socket
    	  :output-sockets '(:audio))))
        
        (cl-synthesizer:add-module
         voice "LFO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency lfo-frequency :v-peak 0.1 :f-max 500.0 :cv-max 5.0)
    
        (cl-synthesizer:add-module
         voice "VCO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency vco-frequency :f-max 5000.0 :v-peak 5.0 :cv-max 5.0)
    
        (cl-synthesizer:add-patch voice "LFO" :sine "VCO" :cv-lin)
        (cl-synthesizer:add-patch voice "VCO" :saw "OUTPUT" :audio)
        
        voice))
      
    (defun example ()
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment)
    	       ;; Expose line-out sockets
    	       :output-sockets '(:left :right))))
    
        (cl-synthesizer:add-module
         rack "VOICE-1" #'make-voice :lfo-frequency 1.0 :vco-frequency 440.0)
        (cl-synthesizer:add-module
         rack "VOICE-2" #'make-voice :lfo-frequency 2.0 :vco-frequency 442.0)
    
        (cl-synthesizer:add-patch rack "VOICE-1" :audio "OUTPUT" :left)
        (cl-synthesizer:add-patch rack "VOICE-2" :audio "OUTPUT" :right)
    
        ;; Generate a Wave-File
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:make-handler
         '(("OUTPUT" :input-socket :left)
           ("OUTPUT" :input-socket :right))
         :filename "rack-example-1.wav")
        
        rack))
    
    (defun run-example ()
      (cl-synthesizer:play-rack (example) :duration-seconds 5))
      
    ;;(run-example)

Installation
------------

    
    cd ~/quicklisp/local-projects
    git clone https://github.com/Frechmatz/cl-synthesizer.git
    (ql:quickload "cl-synthesizer")
    

API Reference
-------------

*   [Environment](#environment)
*   [Rack](#rack)
*   [Modules](#modules)
    *   [VCO](#vco)
    *   [VCA](#vca)
    *   [ADSR](#adsr)
    *   [Multiple](#multiple)
    *   [MIDI Interface](#midi-interface)
    *   [MIDI CC Interface](#midi-cc-interface)
    *   [MIDI Sequencer](#midi-sequencer)
    *   [Fixed Output](#fixed-output)
    *   [Adder](#adder)
    *   [Mixer](#mixer)
    *   [Trigger](#trigger)
    *   [Ramp](#ramp)
    *   [Sustain](#sustain)
    *   [Wave File Writer](#wave-file-writer)
    *   [CSV File Writer](#csv-file-writer)
*   [Monitor](#monitor)
*   [MIDI](#midi)
    *   [MIDI Event](#midi-event)
    *   [MIDI Utilities](#midi-utilities)
*   [Conditions](#conditions)

### Environment

**cl-synthesizer:make-environment** &key (sample-rate 44100.0) (home-directory nil)

Creates an environment. The environment defines properties such as the sample rate of the rack. An enviroment is a property list with the following keys:

*   :sample-rate Sample rate of the synthesizer.
*   :home-directory The base output directory for wave files etc. Default value is the home directory of the current user.

### Rack

**cl-synthesizer:make-rack** &key environment (input-sockets nil) (output-sockets nil)

Creates a rack. A rack is a module container as well as a module. Racks can be added to other racks. The function has the following arguments:

*   :environment The synthesizer environment.
*   :input-sockets The input sockets to be exposed by the rack. The inputs can be patched with other modules via the bridge module "INPUT".
*   :output-sockets The output sockets to be exposed by the rack. The outputs can be patched with other modules via the bridge module "OUTPUT".

The update function calls the update function of all modules. If the rack has already been shut down the function immediately returns **nil**. Othwerwise it returns **t**.

The shutdown function calls the shutdown handlers of all modules and hooks. If the rack has already been shut down the function immediately returns.

See also: add-module

* * *

**cl-synthesizer:is-rack** module

Returns **t** if the given module represents a rack.

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
    *   :update A function that is called with the values of the modules input sockets in order to update the state of the module (the state of its output sockets). All input parameters are passed as a single argument which consists of a property list or nil if the module does not expose any inputs. To avoid excessive consing this list is allocated during compilation of the rack and then used for all update calls of the module.
    *   :get-output A function that is called in order to get the value of a specific output socket. The function is called with a keyword that identifies the output socket whose state is to be returned. The function must not modify the value of the given or any other output socket.
    *   :shutdown An optional function with no arguments that is called when the rack is shutting down.
    *   :get-state An optional function that can be used to expose internal states of the module, for example a VCO may expose its frequency. The function has one argument that consists of a keyword identifying the requested state, for example :frequency.
    
    A module must not add or remove input/output sockets after it has been instantiated.
    
*   &rest args Arbitrary additional arguments to be passed to the module instantiation function. These arguments typically consist of keyword parameters.

Returns the added module.

* * *

**cl-synthesizer:add-patch** rack output-module-name output-socket input-module-name input-socket

Adds a patch to the rack. A patch is an unidirectional connection between an output socket of a source module and an input socket of a destination module. The rack supports cycles which means that an output socket of a module can be patched with one of its inputs (typically via multiple hops through other modules). The function has the following arguments:

*   rack The rack.
*   output-module-name Name of the output (source) module.
*   output-socket A keyword representing one of the output sockets of the output module.
*   input-module-name Name of the input (destination) module.
*   input-socket A keyword representing one of the input sockets of the input module.

The rack signals an assembly-error in the following cases:

*   A module with the given output name does not exist.
*   A module with the given input name does not exist.
*   The given output-socket is already connected with a module.
*   The given output-socket is not exposed by the output module.
*   The given input-socket is already connected with a module.
*   The given input-socket is not exposed by the input module.

* * *

**cl-synthesizer:get-module** rack name

Get a module of a rack. The function has the following arguments:

*   rack The rack.
*   name The name of the module.

Returns the module or nil.

* * *

**cl-synthesizer:get-module-name** rack module

Get the name of a module. The function has the following arguments:

*   rack The rack.
*   module The module.

Returns the name or nil if the module does not belong to the rack

* * *

**cl-synthesizer:find-module** rack module-path

Get a module of a rack. The function has the following arguments:

*   rack The root rack.
*   module-path The path of the module within the rack (through multiple nested racks).  
    Example 1: "VCO"  
    Example 2: '("VOICE-1" "VCO")

Returns nil or a values object consisting of the rack of the module, the module name and the module itself.

* * *

**cl-synthesizer:get-patches** rack

Get all patches of a rack. The function has the following arguments:

*   rack The rack.

Returns a list of property lists with the following keys:

*   :output-name Name of the output module.
*   :output-socket Output socket.
*   :input-name Name of the input module.
*   :input-socket Input socket.

* * *

**cl-synthesizer:get-modules** rack

Get all modules of a rack. The function has the following arguments:

*   rack The rack.

Returns a list of module names

* * *

**cl-synthesizer:play-rack** rack &key duration-seconds

A utility function that "plays" the rack by consecutively calling its update function for a given number of "ticks". The function has the following arguments:

*   rack The rack.
*   :duration-seconds Duration in seconds of how long to play the rack. If for example the duration is 2 seconds and the sample rate of the rack as declared by its environment is 44100, then the update function of the rack will be called 88200 times.

* * *

**cl-synthesizer:get-environment** rack

Returns the environment of the rack.

* * *

**cl-synthesizer:add-hook** rack hook

Adds a hook to the rack. A hook is called each time after the rack has updated its state. A hook consists a property list with the following keys:

*   :update A function with no arguments that is called after the rack has updated its state.
*   :shutdown A function with no arguments that is called when the rack is shutting down.

Hooks must not modify the rack. See also **cl-synthesizer-monitor:add-monitor**.

### Modules

#### VCO

**cl-synthesizer-modules-vco:make-module** name environment &key base-frequency f-max v-peak cv-max (duty-cycle 0.5) (phase-offset 0.0)

Creates a Voltage Controlled Oscillator module with 1V/Octave and linear frequency modulation inputs. The oscillator has through-zero support, as on negative frequencies the phase will move backwards (in clockwise direction). The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :base-frequency The frequency emitted by the oscillator when all frequency control voltages are 0.
*   :f-max The maximum frequency of the oscillator. f-max must be greater than 0.
*   :cv-max The absolute value of the frequency control peak voltage of the :cv-lin input which represents the maximum frequency of the oscillator.
*   :v-peak Absolute value of the output peak voltage emitted by the oscillator.
*   :duty-cycle The duty cycle of the square wave. 0 <= duty-cycle <= 1.
*   :phase-offset A phase offset in radians.

The module has the following inputs:

*   :cv-exp Exponential frequency control voltage. For a given base-frequency of 440Hz a control voltage of 1.0 results in a frequency of 880Hz and a control voltage of -1.0 results in a frequency of 220Hz.
*   :cv-lin Bipolar linear frequency control voltage. Example: If the :f-max of the oscillator is 12000Hz and :cv-max is 5.0V then a :cv-lin of 2.5V results in a frequency of 6000Hz and a :cv-lin of -2.5V results in a frequency of -6000Hz.

The frequency of the oscillator is calculated by adding the frequencies resulting from the :cv-lin and :cv-exp inputs. It is clipped according to the :f-max setting. The module has the following outputs:

*   :sine A sine wave.
*   :triangle A triangle wave.
*   :saw A saw wave.
*   :square A square wave.

The module exposes the following states via the get-state function:

*   :frequency The current frequency of the module.
*   :linear-frequency The current linear frequency part of the module.
*   :exponential-frequency The current exponential frequency part of the module.
*   :phase The current phase in radians (0..2PI).

**Example:**

    (defpackage :cl-synthesizer-modules-vco-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-vco-example-1)
    
    (defun example ()
      "Write all wave forms into a Wave and a Csv file"
      (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
        (cl-synthesizer:add-module
         rack
         "VCO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 10.0 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)
    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:make-handler
         '(("VCO" :output-socket :sine)
           ("VCO" :output-socket :triangle)
           ("VCO" :output-socket :saw)
           ("VCO" :output-socket :square))
         :filename "cl-synthesizer-examples/vco-example-1.wav")
    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-csv-handler:make-handler
         '(("VCO" :output-socket :sine :name "Sine")
           ("VCO" :output-socket :triangle :name "Triangle")
           ("VCO" :output-socket :saw :name "Saw")
           ("VCO" :output-socket :square :name "Square"))
         :filename "cl-synthesizer-examples/vco-example-1.csv"
         :add-header t
         :column-separator ",")
        
        rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (cl-synthesizer:play-rack rack :duration-seconds 1)))
    
    ;; (run-example)

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
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 0.5
         :v-peak 5.0
         :cv-max 5.0
         :f-max 12000.0)
    
        ;; set up oscillator providing the audio signal
        (cl-synthesizer:add-module
         rack "VCO-AUDIO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 10000.0
         :v-peak 5.0
         :cv-max 5.0
         :f-max 12000.0)
    
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
         #'cl-synthesizer-monitor-wave-handler:make-handler
         '(("VCA" :input-socket :cv)
           ("VCA" :input-socket :input)
           ("VCA" :output-socket :output-linear)
           ("VCA" :output-socket :output-exponential))
         :filename "cl-synthesizer-examples/vca-example-1.wav")
        
        rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (cl-synthesizer:play-rack rack :duration-seconds 120)))
    
    ;; (run-example)

#### ADSR

**cl-synthesizer-modules-adsr:make-module** name environment &key attack-time-ms attack-target-output decay-time-ms decay-target-output release-time-ms (time-cv-to-time-ms nil) (gate-threshold 2.5) (backward-coupled nil)

Creates an envelope generator module with the phases Attack, Decay, Sustain and Release. This module has been realized using other modules such as Ramp, Sustain, Trigger and Multiple. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :attack-time-ms Duration of the attack phase in milliseconds.
*   :attack-target-output Target value of the attack phase.
*   :decay-time-ms Duration of the decay phase in milliseconds.
*   :decay-target-output Target value of the decay phase.
*   :release-time-ms Duration of the release phase in milliseconds. The release phase climbs to 0.0.
*   :time-cv-to-time-ms Optional function that converts a time control voltage to a duration in milliseconds (see also Ramp module).
*   :gate-threshold Minimum value of the :gate input that indicates that the gate is on.
*   :backward-coupled If t then the output signal of the envelope will be connected with the input of the attack phase. This can be used to avoid sudden jumps of the envelope as the attack phase by default starts at 0.0.

The module has the following inputs:

*   :gate The gate signal (see also :gate-threshold). The envelope starts working when the gate input switches to "on" and enters into the release phase when it switches to "off".
*   :attack-cv-time Modulates the climbing time of the attack phase (see also Ramp module).
*   :release-cv-time Modulates the climbing time of the release phase (see also Ramp module).

The module has the following outputs:

*   :cv The envelope.

**Example:**

    (defpackage :cl-synthesizer-modules-adsr-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-adsr-example-1)
    
    (defun example ()
      "ADSR example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment))))
        
        (cl-synthesizer:add-module
         rack "MIDI-SEQUENCER"
         #'cl-synthesizer-modules-midi-sequencer:make-module :events
         (list 
          (list :timestamp-milli-seconds 0
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
          (list :timestamp-milli-seconds 1500
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))
    
        (cl-synthesizer:add-module
         rack "MIDI-IFC"
         #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)
    
        (cl-synthesizer:add-module
         rack "ADSR"
         #'cl-synthesizer-modules-adsr:make-module
         :attack-time-ms 500 :attack-target-output 5.0
         :decay-time-ms 250 :decay-target-output 4.0
         :release-time-ms 1000)
        
        (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
        (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-csv-handler:make-handler
         '(("ADSR" :input-socket :gate :name "ADSR Gate In")
           ("ADSR" :output-socket :cv :name "ADSR Out"))
         :filename "cl-synthesizer-examples/adsr-example-1.csv")
        
        rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (cl-synthesizer:play-rack rack :duration-seconds 3)))
    
    ;; (run-example)

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
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 1.0 :v-peak 1.0 :f-max 500.0 :cv-max 5.0)
        
        (cl-synthesizer:add-module rack "MULTIPLE"
    			       #'cl-synthesizer-modules-multiple:make-module :output-count 5)
        (cl-synthesizer:add-patch rack "LFO" :sine "MULTIPLE" :input)
        (cl-synthesizer:add-patch rack "MULTIPLE" :output-1 "OUTPUT" :line-out-1)
        (cl-synthesizer:add-patch rack "MULTIPLE" :output-2 "OUTPUT" :line-out-2)
    
        rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (cl-synthesizer:play-rack rack :duration-seconds 10)))
    
    ;; (run-example)

#### MIDI Interface

**cl-synthesizer-modules-midi-interface:make-module** name environment &key (voice-count 1) (channel nil) (note-number-to-cv (lambda (note-number) (the single-float (/ note-number 12.0)))) (play-mode :play-mode-poly) (cv-gate-on 5.0) (cv-gate-off 0.0) (force-gate-retrigger nil)

Creates a MIDI interface module. The module dispatches MIDI-Note events to so called voices where each voice is represented by a control-voltage and a gate signal. The function has the following arguments:

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

For an example see **midi-sequencer**

* * *

#### MIDI CC Interface

**cl-synthesizer-modules-midi-cc-interface:make-module** name environment &key controller-numbers transform-handler (channel nil) (initial-output 0) (min-output nil) (max-output nil)

Creates a MIDI CC Event interface module. The module maps MIDI control change events to an output value. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :controller-numbers A list of MIDI controller numbers.
*   :transform-handler A function that converts a control value to the output value of the module. It is called for each matching CC event and has the following arguments:
    
    *   The current output value of the module.
    *   Controller number.
    *   Control value.
    
    The function must return the new output value of the module.
    
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
    
    (defun run-example ()
      (cl-synthesizer::play-rack (example) :duration-seconds 5))
    
    ;; (run-example)

#### MIDI Sequencer

**cl-synthesizer-modules-midi-sequencer:make-module** name environment &key events

Creates a Midi-Sequencer module. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :events A list of Midi events and their timestamps. Each entry consists of a property list with the following keys:
    *   :timestamp-milli-seconds Point of time when events are to be fired. The very first timestamp of the synthesizer is 0.
    *   :midi-events List of Midi events to be fired.The events must be ordered by timestamp and there must be no duplicate timestamps.

The module has no inputs. The module has one output socket :midi-events.

**Example:**

    (defpackage :cl-synthesizer-modules-midi-sequencer-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-midi-sequencer-example-1)
    
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
         rack "VCO" #'cl-synthesizer-modules-vco:make-module
         :base-frequency (cl-synthesizer-midi:get-note-number-frequency 0)
         :f-max 12000.0
         :cv-max 5.0
         :v-peak 5.0)
    
        ;; Add ADSR
        (cl-synthesizer:add-module
         rack "ADSR"
         #'cl-synthesizer-modules-adsr:make-module
         :attack-time-ms 100 :attack-target-output 5.0
         :decay-time-ms 50 :decay-target-output 3.0
         :release-time-ms 100)
        
        ;; Add VCA
        (cl-synthesizer:add-module rack "VCA" #'cl-synthesizer-modules-vca:make-module :cv-max 5.0)
    
        ;; Connect VCA with ADSR and VCO
        (cl-synthesizer:add-patch rack "VCA" :output-linear "OUTPUT" :line-out)
        (cl-synthesizer:add-patch rack "ADSR" :cv "VCA" :cv)
        (cl-synthesizer:add-patch rack "VCO" :triangle "VCA" :input)
        
        ;; Connect Midi interface with ADSR and VCO
        (cl-synthesizer:add-patch rack "MIDI-IFC" :cv-1 "VCO" :cv-exp)
        (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "ADSR" :gate)
    
        ;; Record LINE-OUT into a wave file
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:make-handler
         '(("OUTPUT" :input-socket :line-out))
         :filename "cl-synthesizer-examples/midi-sequencer-example-1.wav")
        
        rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (cl-synthesizer::play-rack rack :duration-seconds 5)))
    
    ;; (run-example)

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
    
    (defun run-example ()
      (cl-synthesizer:play-rack (example) :duration-seconds 1))
    
    ;; (run-example)

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

#### Trigger

**cl-synthesizer-modules-trigger:make-module** name environment &key trigger-threshold pulse-voltage

Creates a Voltage to Trigger Converter module. The module fires a one clock cycle long pulse when input voltage >= trigger-threshold and then waits that the input voltage descends below trigger-threshold before the next pulse can be triggered. The module can for example be used to generate a trigger out of a gate signal. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :trigger-threshold The minimum value of the input which triggers a pulse.
*   :pulse-voltage The voltage of the pulse.

The module has the following inputs:

*   :input The input voltage.

The module has the following outputs:

*   :output The output voltage (zero or pulse-voltage).

**Example:**

    (defpackage :cl-synthesizer-modules-trigger-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-trigger-example-1)
    
    (defun example ()
      "Emit trigger signal based on sine input"
      (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
        (cl-synthesizer:add-module
         rack
         "VCO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 5.0 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)
    
        (cl-synthesizer:add-module
         rack
         "TRIGGER"
         #'cl-synthesizer-modules-trigger:make-module
         :trigger-threshold 4.9 :pulse-voltage 3.0)
    
        (cl-synthesizer:add-patch rack "VCO" :sine "TRIGGER" :input)
        
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-wave-handler:make-handler
         '(("TRIGGER" :input-socket :input)
           ("TRIGGER" :output-socket :output))
         :filename "cl-synthesizer-examples/trigger-example-1.wav")
    
        rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (cl-synthesizer:play-rack rack :duration-seconds 2)))
    
    ;; (run-example)

#### Ramp

**cl-synthesizer-modules-ramp:make-module** name environment &key time-ms target-output (gate-state nil) (trigger-threshold 2.5) (gate-threshold 2.5) (time-cv-to-time-ms nil)

Creates a module whose output climbs from a given input value to a given output value in a given time. Main purpose of this module is to create envelope generators by chaining multiple ramp and sustain modules. The module climbs linearly (exponential climbing will be added later). The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :time-ms Default climbing time (duration) in milliseconds.
*   :target-output Desired target output value. Due to the time resolution given by the sample-rate of the environment the ramp may stop at an output value a little bit smaller or greater than the desired target-output value.
*   :gate-state Required state of the Gate input. One of :on, :off, nil
*   :trigger-threshold Minimum value of the :trigger input that indicates that the trigger is active.
*   :gate-threshold Minimum value of the :gate input that indicates that the gate is on.
*   :time-cv-to-time-ms An optional function that converts a time control voltage to a duration in milliseconds. The default implementation is 1000ms/1V (abs(cv-time) \* 1000).

The module has the following inputs:

*   :trigger Trigger input. If the trigger is active (see also :trigger-threshold), the module samples its current input value and begins climbing to the desired target output value.
*   :input Input value.
*   :pass-through If value is >= 5.0 the module passes through its input value.
*   :gate A gate signal (see also :gate-threshold).
*   :cv-time NIL or climbing time (duration) of the ramp (see also :time-cv-to-time-ms).

The module has the following outputs:

*   :output Output value of the module. The initial output value is 0.0.
*   :busy A value >= 5.0 indicates that the module is busy by either passing through its input value or climbing to the target output value.
*   :done A trigger signal that jumps to 5.0 for the length of one clock cycle when the ramp has finished.
*   :gate Passed through :gate input. Purpose of this output is to support more convenient chaining of ramp and sustain modules.

When the ramp aborts due to a toggling Gate signal or when its supposed duration has been exceeded due to time modulation then the output value does not jump to the desired target-output but stays at its current value.  
  
This module has been inspired by [dhemery](https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes)

**Example:**

    (defpackage :cl-synthesizer-modules-ramp-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-ramp-example-1)
    
    (defun example ()
      "Ramp example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment))))
        
        (cl-synthesizer:add-module
         rack "VCO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 0.5 :v-peak 5.0 :f-max 500.0 :cv-max 5.0)
    
        (cl-synthesizer:add-module
         rack "TRIGGER"
         #'cl-synthesizer-modules-trigger:make-module
         :trigger-threshold 4.9 :pulse-voltage 5.0)
    
        (cl-synthesizer:add-module
         rack "ATTACK"
         #'cl-synthesizer-modules-ramp:make-module
         :time-ms 200 :target-output 5.0 :gate-state nil)
    
        (cl-synthesizer:add-module
         rack "DECAY"
         #'cl-synthesizer-modules-ramp:make-module
         :time-ms 200 :target-output 2.5)
        
        (cl-synthesizer:add-patch rack "VCO" :square "TRIGGER" :input)
        (cl-synthesizer:add-patch rack "TRIGGER" :output "ATTACK" :trigger)
        (cl-synthesizer:add-patch rack "ATTACK" :busy "DECAY" :pass-through)
        (cl-synthesizer:add-patch rack "ATTACK" :output "DECAY" :input)
        (cl-synthesizer:add-patch rack "ATTACK" :done "DECAY" :trigger)
    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-csv-handler:make-handler
         '(("VCO" :output-socket :square :name "VCO Out")
           ("ATTACK" :output-socket :output :name "Attack Out")
           ("DECAY" :output-socket :output :name "Decay Out"))
         :filename "cl-synthesizer-examples/ramp-example-1.csv")
        
        rack))
    
    (defun run-example ()
      (let ((rack (example))) (cl-synthesizer:play-rack rack :duration-seconds 5)))
    
    ;; (run-example)

#### Sustain

**cl-synthesizer-modules-sustain:make-module** name environment &key (trigger-threshold 2.5) (gate-threshold 2.5)

Creates a module which holds a given input as long as its gate input is "on". Main purpose of this module is to create envelope generators by chaining multiple ramp and sustain modules. The function has the following arguments:

*   name Name of the module.
*   environment The synthesizer environment.
*   :trigger-threshold Minimum value of the :trigger input that indicates that the trigger is active.
*   :gate-threshold Minimum value of the :gate input that indicates that the gate is on.

The module has the following inputs:

*   :trigger Trigger input. If the trigger is active (see also :trigger-threshold), the module samples its current input value and begins passing it to its output socket.
*   :input Input value.
*   :pass-through If value is >= 5.0 the module passes through its input value.
*   :gate A gate signal (see also :gate-threshold).

The module has the following outputs:

*   :output Output value of the module. The initial output value is 0.0.
*   :busy A value >= 5.0 indicates that the module is busy by either passing through its input value or holding the sampled input value until the gate input falls to zero.
*   :done A trigger signal that jumps to 5.0 for the length of one clock cycle when the sustain cycle has finished.
*   :gate Passed through :gate input. Purpose of this output is to support more convenient chaining of ramp and sustain modules.

This module has been inspired by [dhemery](https://github.com/dhemery/DHE-Modules/wiki/Multi-Stage-Envelopes)

**Example:**

    (defpackage :cl-synthesizer-modules-sustain-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-modules-sustain-example-1)
    
    (defun example ()
      "Sustain example"
      (let ((rack (cl-synthesizer:make-rack
    	       :environment (cl-synthesizer:make-environment))))
        
        ;; Use MIDI sequencer for generation of Gate signals
        (cl-synthesizer:add-module
         rack "MIDI-SEQUENCER"
         #'cl-synthesizer-modules-midi-sequencer:make-module :events
         (list 
          (list :timestamp-milli-seconds 300
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
          (list :timestamp-milli-seconds 700
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))
          (list :timestamp-milli-seconds 1800
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-on-event 1 69 100)))
          (list :timestamp-milli-seconds 2100
    	    :midi-events (list
    			  (cl-synthesizer-midi-event:make-note-off-event 1 69 100)))))
    
        (cl-synthesizer:add-module
         rack "MIDI-IFC"
         #'cl-synthesizer-modules-midi-interface:make-module :voice-count 1)
    
        (cl-synthesizer:add-module
         rack "GATE-MULTIPLE"
         #'cl-synthesizer-modules-multiple:make-module :output-count 2)
    
        (cl-synthesizer:add-module
         rack "TRIGGER"
         #'cl-synthesizer-modules-trigger:make-module
         :trigger-threshold 4.9 :pulse-voltage 5.0)
    
        (cl-synthesizer:add-module
         rack "VCO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 0.5 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)
        
        (cl-synthesizer:add-module
         rack "SUSTAIN"
         #'cl-synthesizer-modules-sustain:make-module)
    
        (cl-synthesizer:add-patch rack "MIDI-SEQUENCER" :midi-events "MIDI-IFC" :midi-events)
        (cl-synthesizer:add-patch rack "MIDI-IFC" :gate-1 "GATE-MULTIPLE" :input)
        (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-1 "TRIGGER" :input)
        (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-2 "SUSTAIN" :gate)
        (cl-synthesizer:add-patch rack "TRIGGER" :output "SUSTAIN" :trigger)
        (cl-synthesizer:add-patch rack "VCO" :sine "SUSTAIN" :input)
    
        (cl-synthesizer-monitor:add-monitor
         rack
         #'cl-synthesizer-monitor-csv-handler:make-handler
         '(("MIDI-IFC" :output-socket :gate-1 :name "Gate")
           ("SUSTAIN" :input-socket :trigger :name "Sustain Trigger In")
           ("SUSTAIN" :input-socket :input :name "Sustain In")
           ("SUSTAIN" :output-socket :output :name "Sustain Out")
           ("SUSTAIN" :output-socket :done :name "Sustain Done Out"))
         :filename "cl-synthesizer-examples/sustain-example-1.csv")
        
        rack))
    
    (defun run-example ()
      (let ((rack (example))) (cl-synthesizer:play-rack rack :duration-seconds 3)))
    
    ;; (run-example)

#### Wave File Writer

**cl-synthesizer-modules-wave-file-writer:make-module** name environment &key channel-count filename (v-peak 5.0)

Creates a Wave File Writer module. Writes files in "Waveform Audio File" ("WAV") format. The function has the following arguments:

*   name Name of the writer.
*   environment The synthesizer environment.
*   :channel-count Number of channels.
*   :filename The relative path of the file to be written. The filename will be concatenated with the base path as defined by the :home-directory property of the environment.
*   :v-peak Optional peak voltage. The inputs of the module will be scaled to v-peak. If for example v-peak is set to 20.0 an incoming voltage of 5.0 results in a sample value (which is written into the wave file) of 5.0 / 20.0 -> 0.25 and an incoming voltage of -5.0 results in a sample value of -0.25. The default value is 5.0. Incoming voltages will be clipped according to v-peak.

The module has the following inputs:

*   :channel-1 ... :channel-n The sample values of the generated frames are written in order :channel-1 ... :channel-n

The module has no outputs.

See also cl-synthesizer-monitor:add-monitor which provides Wave-File-Writing without having to add the module and the required patches to the rack.

#### CSV File Writer

**cl-synthesizer-modules-csv-file-writer:make-module** name environment &key columns filename (column-separator ",") (add-header t)

Creates a CSV File Writer module. The function has the following arguments:

*   name Name of the writer.
*   environment The synthesizer environment.
*   :columns A list of column definitions. Each colum definition consists of a property list with the following keys:
    *   :name Name of the column.
    *   :default-value Default value to be used if current column value is nil.
*   :filename The relative path of the file to be written. The filename will be concatenated with the base path as defined by the :home-directory property of the environment.
*   :column-separator The column separator.
*   :add-header If t then a header consisting of the column-names will be written.

The module has the following inputs:

*   :column-1 ... :column-n Where n is the number of columns.

Due to performance/consing considerations all columns are written using the Lisp-Writer. If a value contains the column separator it will not be quoted. The file is opened on the first call of the update function and closed by the shutdown handler.

The module has no outputs.

See also cl-synthesizer-monitor:add-monitor

### Monitor

**cl-synthesizer-monitor:add-monitor** rack monitor-handler socket-mappings &rest additional-handler-args

Adds a monitor to a rack. A monitor is a high-level Rack hook that collects module states (values of input/output sockets) and passes them to a monitor handler. A monitor handler can for example be a Wave-File-Writer. The function has the following arguments:

*   rack The rack.
*   monitor-handler A function that instantiates the monitor handler. This function is called with the following arguments:
    
    *   name A name.
    *   environment The synthesizer environment.
    *   inputs A list of inputs. Each entry consists of the additional settings that have been set at a specific socket mapping, for example the CSV formatting string.
    *   additional-handler-args Any additional keyword parameters as passed to the monitor function. These parameters can be used to initialize handler specific properties such as a filename.
    
    The function must return a values object with the following entries:
    *   module A property list that represents a subset of a module. At least :update must be implemented. See also cl-synthesizer:add-module.
    *   An ordered list of input sockets of the module, where the first entry represents the first entry of the socket mappings (e.g. column-1) and so on. This list is in place because we do not want to depend on the actual input sockets exposed by the module. It is up to the monitor-handler to know about specifica of modules, for example that the csv-file-writer module uses input socket :column-1 to represent the first column.
*   socket-mappings Declares the input/outputs whose values are to be monitored. Each entry has the following format:
    *   module-path Path of the module from which the value of a certain input/output socket or state is to be retrieved, for example "ADSR" or '("VOICE-1" "ADSR"). See also cl-synthesizer:find-module.
    *   socket-type One of the following keywords:
        *   :input-socket Monitor the value of an input socket of the module.
        *   :output-socket Monitor the value of an output socket of the module.
        *   :state Monitor a state of the module (see get-state function).
    *   socket A keyword that identifies one of the input/output sockets or states provided by the module, for example :cv
    *   Any additional settings. Supported settings depend on the handler that is being used, for example a CSV writer may support a column formatting string.
*   &rest additional-handler-args Optional keyword arguments to be passed to the handler instantiation function.

**Example:**

    (defpackage :cl-synthesizer-monitor-example-1
      (:use :cl))
    
    (in-package :cl-synthesizer-monitor-example-1)
    
    (defun example ()
      "Monitor example"
      (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment))))
        
        (cl-synthesizer:add-module
         rack
         "VCO"
         #'cl-synthesizer-modules-vco:make-module
         :base-frequency 10.0 :v-peak 5.0 :cv-max 5.0 :f-max 12000.0)
    
        (flet ((instantiate-handler (name environment inputs)
    	     (declare (ignore name environment inputs))
    	     (values 
    	      (list
    	       :update (lambda (input-args)
    			 (format t "~%Sine: ~a" (getf input-args :sine))
    			 (format t "~%Phase: ~a" (getf input-args :phase))))
    	       '(:sine :phase))))
          
          (cl-synthesizer-monitor:add-monitor
           rack
           #'instantiate-handler
           '(("VCO" :output-socket :sine)
    	 ("VCO" :state :phase))))
        
      rack))
    
    (defun run-example ()
      (let ((rack (example)))
        (funcall (getf rack :update) nil)
        (funcall (getf rack :update) nil)))
    
    ;; (run-example)

**cl-synthesizer-monitor-wave-handler:make-handler** name environment inputs &rest rest &key filename &allow-other-keys

Creates a monitor handler which writes its inputs into a Wave file. The function has the following arguments:

*   name A name.
*   environment The synthesizer environment.
*   inputs The column input settings as provided by the Monitor component.
*   :filename A file path relative to the output directory as defined by the environment.

See also cl-synthesizer-modules:wave-file-writer.

* * *

**cl-synthesizer-monitor-csv-handler:make-handler** name environment inputs &rest rest &key filename &allow-other-keys

Creates a monitor handler which writes its inputs into a CSV file. The function has the following arguments:

*   name A name.
*   environment The synthesizer environment.
*   inputs The column input settings as provided by the Monitor component.
*   :filename A file path relative to the output directory as defined by the environment.

See also cl-synthesizer-modules:csv-file-writer.

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

Acknowledgements
----------------

The Wave-Writer module uses code taken from Ryan Kings [cl-wave](https://github.com/RyanTKing/cl-wave) library.

* * *

Generated 2019-02-14 23:32:58