(in-package :cl-synthesizer-modules-midi-relative-cc-interface)

(defun make-module (name environment
		    &key mappings
		      (channel nil)
		      (initial-output 0)
		      (min-output nil)
		      (max-output nil))
  "Creates a MIDI CC Event interface module. CC events are interpreted as relative 
   changes to the current output value.
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:mappings A list of mappings. A mapping consists of a controller number, control values and a resulting offset 
          that will be added to the current output value of the module. A mapping can be defined as shown by the following examples:
          <ul>
             <li>(:controller-number 112 :control-value 61 :offset 1.0)</li>
             <li>(:controller-number 112 :control-value (:range 60 70) :offset 1.0)</li>
             <li>(:controller-number 112 :control-value (61 63 65) :offset 1.0)</li>
          </ul>
        </li>
        <li>:channel Optional number of the MIDI channel to which the controller events 
          must belong. By default there is no channel filtering applied.</li>
        <li>:initial-output The initial output value of the module.</li>
        <li>:min-output Optional lowest output value of the module.</li>
        <li>:max-output Optional largest output value of the module.</li>
    </ul></p>
    <p>The module has the following inputs:
    <ul>
	<li>:midi-events A list of MIDI events.</li>
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:output The current output value.</li>
    </ul></p>"
  (declare (ignore environment))
  (if (not mappings)
      (cl-synthesizer:signal-assembly-error
       :format-control "Mappings must not be nil '~a'"
       :format-arguments (list name)))
  (let ((cc-mapper (cl-synthesizer-midi::cc-mapper)))
    (dolist (mapping mappings)
      (if (not (listp mapping))
	    (cl-synthesizer:signal-assembly-error
	     :format-control "Mapping is not a list '~a'"
	     :format-arguments (list mapping)))
      (let ((controller-number (getf mapping :controller-number))
	    (control-value (getf mapping :control-value))
	    (offset (getf mapping :offset)))
	(if (not controller-number)
	    (cl-synthesizer:signal-assembly-error
	     :format-control "No controller-number set at mapping '~a'"
	     :format-arguments (list mapping)))
	(if (not offset)
	    (cl-synthesizer:signal-assembly-error
	     :format-control "No offset set at mapping '~a'"
	     :format-arguments (list mapping)))
	(if (not control-value)
	    (cl-synthesizer:signal-assembly-error
	     :format-control "No control-value(s) set at mapping '~a'"
	     :format-arguments (list mapping)))
	(cond
	  ((not (listp control-value))
	   (funcall (getf cc-mapper :add-mapping) controller-number control-value offset))
	  ((eq (first control-value) :range)
	   (funcall (getf cc-mapper :add-range-mapping) controller-number (second control-value) (third control-value) offset))
	  (t
	   (funcall (getf cc-mapper :add-list-mapping) controller-number control-value offset)))))
    (flet ((clip (v)
	     (cond
	       ((not v) v)
	       ((and min-output (> min-output v)) min-output)
	       ((and max-output (< max-output v)) max-output)
	       (t v))))
      (let* ((cur-output (clip initial-output)) (input-midi-events nil)
	     (inputs (list :midi-events (lambda(value) (setf input-midi-events value))))
	     (outputs (list :output (lambda() cur-output)))
	     (map-fn (getf cc-mapper :map)))
	(list
	 :inputs (lambda () inputs)
	 :outputs (lambda () outputs)
	 :update (lambda ()
		   (dolist (midi-event input-midi-events)
		     (if (and midi-event
			      (cl-synthesizer-midi-event:control-change-eventp midi-event)
			      (or (not channel)
				  (= channel (cl-synthesizer-midi-event:get-channel midi-event))))
			 (let ((offset
				 (funcall
				  map-fn
				  (cl-synthesizer-midi-event:get-controller-number midi-event)
				  (cl-synthesizer-midi-event:get-control-value midi-event))))
			   (if offset
			       (setf cur-output (clip (+ cur-output offset)))))))))))))

