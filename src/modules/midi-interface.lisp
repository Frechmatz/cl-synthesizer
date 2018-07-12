(in-package :cl-synthesizer-modules-midi-interface)

;;
;;
;; A Midi Rack Module
;;
;; Work in progress
;;

(defconstant +voice-state-cv+ 0)
(defconstant +voice-state-gate+ 1)
(defconstant +voice-state-gate-retrigger+ 2)

(defparameter +voice-states+
  '(+voice-state-cv+
    +voice-state-gate+
    +voice-state-gate-retrigger+))

(defun make-voice-state (name environment voice-number)
  (declare (ignore name environment voice-number))
  (let ((voice-state (make-array (length +voice-states+))))
    (setf (elt voice-state +voice-state-cv+) 0)
    (setf (elt voice-state +voice-state-gate+) 0)
    (setf (elt voice-state +voice-state-gate-retrigger+) nil)
    voice-state))

(defun get-voice-state-cv (state) (elt state +voice-state-cv+))
(defun set-voice-state-cv (state cv) (setf (elt state +voice-state-cv+) cv))
(defun get-voice-state-gate (state) (elt state +voice-state-gate+))
(defun set-voice-state-gate (state cv) (setf (elt state +voice-state-gate+) cv))
(defun get-voice-state-gate-retrigger (state) (elt state +voice-state-gate-retrigger+))
(defun set-voice-state-gate-retrigger (state cv) (setf (elt state +voice-state-gate-retrigger+) cv))

(defun validate-controller (controller module-outputs)
  (let ((output-keyword (getf controller :socket)))
    (if (not (keywordp output-keyword))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller handler output socket ~a must be a keyword"
	 :format-arguments (list output-keyword)))
    (if (find output-keyword module-outputs)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller handler output socket ~a is already defined"
	 :format-arguments (list output-keyword))))
  (if (or (not (listp (getf controller :handler))) (= 0 (length (getf controller :handler))))
      (cl-synthesizer:signal-assembly-error
       :format-control "Controller handler object ~a must be a non-empty list"
       :format-arguments (list controller)))
  (if (not (getf (getf controller :handler) :get-output))
      (cl-synthesizer:signal-assembly-error
       :format-control "Controller handler object ~a must provide a 'get-output' function property"
       :format-arguments (list controller)))
  (if (not (getf (getf controller :handler) :update))
      (cl-synthesizer:signal-assembly-error
       :format-control "Controller handler object ~a must provide a 'update' function property"
       :format-arguments (list controller))))

(defun midi-interface (name environment
		       &key
			 (voice-count 1)
			 (channel nil)
			 (note-number-to-cv (lambda (note-number) (/ note-number 12)))
			 (play-mode :PLAY-MODE-POLY)
			 (cv-gate-on 5.0)
			 (cv-gate-off 0.0)
			 (controllers nil))
  "Creates a MIDI interface module. The module maps MIDI events to so called voices where each
    voice is represented by a control-voltage and a gate signal. The module supports the
    mapping of MIDI CC-Events to arbitary output sockets. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:voice-count The number of voices to be exposed by the module. Each voice consists
	    of the following output sockets:
	    <ul>
		<li>:GATE-n The gate signal, for example :GATE-1 n = 1..voice-count.</li>
		<li>:CV-n A control voltage representing the note number, for example :CV-1.
		    n = 1..voice-count</li>
	    </ul>
	    The default voice count is 1.</li>
	<li>:channel Optional MIDI channel to which note events must belong. By default the
	    channel is ignored. This setting does not effect the evaluation of
	    CC-Events that are handled by controllers. Controllers must implement
	    channel filtering on their own.</li>
	<li>:note-number-to-cv An optional function that is called with a MIDI note number
	    and returns a control-voltage. The default implementation is
	    (lambda (note-number) (/ note-number 12)))</li>
	<li>:play-mode One of
	    <ul>
		<li>:PLAY-MODE-POLY Polyphonic play mode. Incoming note events will be
		    dispatched to \"available\" voices, where a voice is available
		    when it meets certain criteria. These criteria are defined
		    and implemented by the cl-synthesizer-midi-voice-manager:voice-manager
		    package.</li>
		<li>:PLAY-MODE-UNISONO Monophonic play mode. All voices exposed by the module
		    are set to the current \"active\" note. Notes are stacked. When a note is
		    released, the voice outputs switch to the previous note. This logic is
		    also implemented by the cl-synthesizer-midi-voice-manager:voice-manager
		    package.</li>
	    </ul>
	    The default value is :PLAY-MODE-POLY</li>
	<li>:cv-gate-on The \"Gate on\" control voltage. The default value is 5.0</li>
	<li>:cv-gate-off The \"Gate off\" control voltage. The default value is 0.0</li>
	<li>:controllers Controllers can be used to declare additional output sockets that are
	    exposed by the module. The controllers parameter consists of a list of property lists
	    with the following keys:
	    <ul>
		<li>:socket A keyword that defines the output socket to be exposed by the modules.</li>
		<li>:handler A property list that defines the keys
		    <ul>
			<li>:update A function that is called with the MIDI events passed to the update
			    function of the module.</li>
			<li>:get-output A function with no arguments that returns the current value
			    of the controller.</li>
		    </ul>
		    For typical use cases refer to cl-synthesizer-midi:relative-cc-handler
		</li>
	    </ul>
	</li>
    </ul>
    The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul>
    The module has the following outputs:
    <ul>
	<li>:GATE-1 ... :GATE-n</li>
	<li>:CV-1 ... :CV-n</li>
	<li>Outputs as defined by controllers</li>
    </ul>
    Example:
    <pre><code>
    (cl-synthesizer:add-module
        rack
        \"MIDI-IFC\"
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
    </code></pre>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let* ((outputs nil)
	 (voice-states (make-array voice-count))
	 (output-socket-lookup-table (make-hash-table :test #'eq))
	 (voice-manager (make-instance
			 'cl-synthesizer-midi-voice-manager:voice-manager
			 :voice-count (if (eq play-mode :PLAY-MODE-POLY) voice-count 1))))
    (dotimes (i voice-count)
      (let ((cv-socket (cl-synthesizer-macro-util:make-keyword "CV" i))
	    (gate-socket (cl-synthesizer-macro-util:make-keyword "GATE" i)))
	(setf outputs (push cv-socket outputs))
	(setf outputs (push gate-socket outputs))
	;; in Unisono mode all voices share the same state object
	(if (or (= 0 i) (not (eq play-mode :PLAY-MODE-UNISONO)))
	    (setf (elt voice-states i) (make-voice-state name environment i))
	    (setf (elt voice-states i) (elt voice-states 0)))
	(let ((cur-i i)) ;; new context
	  (setf (gethash cv-socket output-socket-lookup-table)
		(lambda () (get-voice-state-cv (elt voice-states cur-i))))
	  (setf (gethash gate-socket output-socket-lookup-table)
		(lambda () (get-voice-state-gate (elt voice-states cur-i)))))))
    ;; process controllers
    (dolist (controller controllers)
      (validate-controller controller outputs)
      (setf outputs (push (getf controller :socket) outputs))
      (let ((cur-controller controller)) ;; new context
	(setf (gethash (getf cur-controller :socket) output-socket-lookup-table)
	      (lambda () (funcall (getf (getf cur-controller :handler) :get-output))))))
    (list
     :inputs (lambda () '(:midi-events))
     :outputs (lambda () outputs)
     :get-output (lambda (output)
		   (let ((handler (gethash output output-socket-lookup-table)))
		     (if (not handler)
			 (error (format nil "Unknown input ~a requested from ~a" output name)))
		     (funcall handler)))
     :update (lambda (&key (midi-events nil))
	       ;; Update controllers
	       (dolist (c controllers)
		 (funcall (getf (getf c :handler) :update) midi-events))
	       ;; Update voices
	       (dolist (midi-event midi-events)
		 (if (and midi-event
			  (or (not channel)
			      (= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		     (cond
		       ((cl-synthesizer-midi-event:note-on-eventp midi-event)
			(multiple-value-bind (voice-index voice-note stack-size)
			    (cl-synthesizer-midi-voice-manager:push-note
			     voice-manager
			     (cl-synthesizer-midi-event:get-note-number midi-event))
			  (let ((voice-state (elt voice-states voice-index)))
			    ;; if first note then set gate to on
			    (if (= 1 stack-size)
				(set-voice-state-gate voice-state cv-gate-on))
			    (set-voice-state-cv
			     voice-state
			     (funcall note-number-to-cv voice-note)))))
		       ((cl-synthesizer-midi-event:note-off-eventp midi-event)
			(multiple-value-bind (voice-index voice-note)
			    (cl-synthesizer-midi-voice-manager:remove-note
			     voice-manager
			     (cl-synthesizer-midi-event:get-note-number midi-event))
			  (if voice-index
			      (let ((voice-state (elt voice-states voice-index)))
				;; if no note left then set gate to off
				(if (not voice-note)
				    (set-voice-state-gate voice-state cv-gate-off)
				    (set-voice-state-cv
				     voice-state
				     (funcall note-number-to-cv voice-note))))))))))))))
