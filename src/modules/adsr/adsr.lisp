(in-package :cl-synthesizer-modules-adsr)

(defun make-module (name environment
		    &key
		      attack-time-ms attack-target-output
		      decay-time-ms decay-target-output
		      release-time-ms
		      (time-cv-to-time-ms nil)
		      (gate-threshold 2.5)
		      (backward-coupled nil)
		      (exponential nil))
  "Creates an envelope generator module with the phases Attack, Decay, Sustain and Release.
    <p>The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:attack-time-ms Duration of the attack phase in milliseconds.</li>
	<li>:attack-target-output Target value of the attack phase.</li>
	<li>:decay-time-ms Duration of the decay phase in milliseconds.</li>
	<li>:decay-target-output Target value of the decay phase.</li>
	<li>:release-time-ms Duration of the release phase in milliseconds. The release phase climbs to 0.0.</li>
	<li>:time-cv-to-time-ms Optional function that converts a time control voltage to a duration in milliseconds. The default implementation is 1000ms/1V.</li>
	<li>:gate-threshold Minimum value of the :gate input that indicates that the gate is on.</li>
        <li>:backward-coupled If t then the output signal of the envelope will be connected with 
            the input of the attack phase. This can be used to avoid sudden jumps of the envelope 
            as the attack phase by default starts at 0.0.</li>
        <li>:exponential If t then the envelope will have exponential characteristic.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:gate The gate signal (see also :gate-threshold). The envelope starts working when the
	gate input switches to \"on\" and enters into the release phase when it switches to \"off\".</li>
	<li>:attack-cv-time Modulates the climbing time of the attack phase (see also :time-cv-to-time-ms).</li>
	<li>:release-cv-time Modulates the climbing time of the release phase (see also :time-cv-to-time-ms).</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:cv The envelope.</li>
    </ul></p>"
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack
	       :environment environment
	       :input-sockets '(:gate :attack-cv-time :release-cv-time) 
	       :output-sockets '(:cv))))

    (cl-synthesizer:add-module
     rack
     "GATE-MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module :output-count 2)

    (cl-synthesizer:add-module
     rack "TRIGGER"
     #'cl-synthesizer-modules-trigger:make-module
     :trigger-threshold 4.9 :pulse-voltage 5.0)

    (cl-synthesizer:add-module
     rack "ATTACK"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms attack-time-ms :target-output attack-target-output
     :gate-state :on
     :time-cv-to-time-ms time-cv-to-time-ms
     :gate-threshold gate-threshold
     :exponential exponential)
    (cl-synthesizer:add-patch rack "INPUT" :attack-cv-time "ATTACK" :cv-time)
    
    (cl-synthesizer:add-module
     rack "DECAY"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms decay-time-ms :target-output decay-target-output
     :gate-state :on
     :time-cv-to-time-ms time-cv-to-time-ms
     :gate-threshold gate-threshold
     :exponential exponential)
    
    (cl-synthesizer:add-module
     rack "SUSTAIN"
     #'cl-synthesizer-modules-sustain:make-module
     :gate-threshold gate-threshold)
    
    (cl-synthesizer:add-module
     rack "RELEASE"
     #'cl-synthesizer-modules-ramp:make-module
     :time-ms release-time-ms :target-output 0.0 :time-cv-to-time-ms time-cv-to-time-ms
     :gate-threshold gate-threshold
     :exponential exponential)
    (cl-synthesizer:add-patch rack "INPUT" :release-cv-time "RELEASE" :cv-time)

    ;; Inputs => ATTACK
    (cl-synthesizer:add-patch rack "INPUT" :gate "GATE-MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-1 "TRIGGER" :input)
    (cl-synthesizer:add-patch rack "GATE-MULTIPLE" :output-2 "ATTACK" :gate)
    (cl-synthesizer:add-patch rack "TRIGGER" :output "ATTACK" :trigger)
    
    ;; ATTACK => DECAY
    (cl-synthesizer:add-patch rack "ATTACK" :busy "DECAY" :pass-through)
    (cl-synthesizer:add-patch rack "ATTACK" :output "DECAY" :input)
    (cl-synthesizer:add-patch rack "ATTACK" :gate "DECAY" :gate)
    (cl-synthesizer:add-patch rack "ATTACK" :done "DECAY" :trigger)
    
    ;; DECAY => SUSTAIN
    (cl-synthesizer:add-patch rack "DECAY" :busy "SUSTAIN" :pass-through)
    (cl-synthesizer:add-patch rack "DECAY" :output "SUSTAIN" :input)
    (cl-synthesizer:add-patch rack "DECAY" :gate "SUSTAIN" :gate)
    (cl-synthesizer:add-patch rack "DECAY" :done "SUSTAIN" :trigger)

    ;; SUSTAIN => RELEASE
    (cl-synthesizer:add-patch rack "SUSTAIN" :busy "RELEASE" :pass-through)
    (cl-synthesizer:add-patch rack "SUSTAIN" :output "RELEASE" :input)
    (cl-synthesizer:add-patch rack "SUSTAIN" :gate "RELEASE" :gate)
    (cl-synthesizer:add-patch rack "SUSTAIN" :done "RELEASE" :trigger)

    (cl-synthesizer:add-module
     rack
     "OUTPUT-MULTIPLE"
     #'cl-synthesizer-modules-multiple:make-module :output-count 2)
    (cl-synthesizer:add-patch rack "RELEASE" :output "OUTPUT-MULTIPLE" :input)
    (cl-synthesizer:add-patch rack "OUTPUT-MULTIPLE" :output-1 "OUTPUT" :cv)
    (if backward-coupled
	(cl-synthesizer:add-patch rack "OUTPUT-MULTIPLE" :output-2 "ATTACK" :input))
	
    rack))
