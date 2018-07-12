(in-package :cl-synthesizer-midi)

(defun relative-cc-handler (midi-controller controllers &key cv-initial cv-min cv-max (channel nil))
  "Creates a handler function that maps MIDI events of n >= 1 relative MIDI CC-Controllers
  to a single target value. The generator function has the following arguments:
  <ul>
    <li>midi-controller A property list with the keys
      <ul>
	  <li>:get-controller-number A function with one argument that is called with a
	      keyword that identifies the controller, for example :ENCODER-1 and returns
	      the controller number, for example 112.</li>
	  <li>:get-controller-value-offset A function with one argument that is called with
	      the value of a relative CC event, for example 62, and returns a positive or
	      negative offset, for example -3.</li>
      </ul>
      <li>controllers A list of property lists with the keys
	  <ul>
	      <li>:controller-id A keyword that identifies an encoder of the given midi-controller, 
		  for example :ENCODER-1</li>
	      <li>:delta-percent The \"weight\" of the controller that defines how much the control-voltage will 
		  be increased/decreased when the controller is turned. The value is relative to the total 
		  control voltage range as defined by cv-min and cv-max.</li>
	      <li>:turn-speed An optional function that is called with the absolute value of the increase/decrease
		  offset as returned by the :get-controller-value-offset function of the midi-controller. The offset 
		  typically depends on the speed with which the encoder is turned. The function must return the 
		  absolute value of the new offset. This function can for example be used to disable turn-speed 
		  specific increments/decrements by simply returning 1.
		  Example: (:turn-speed (lambda (offs) 1))</li>
	  </ul>
      </li>
      <li>:cv-initial The initial output value of the handler function.</li>
      <li>:cv-min The minimum output value of the handler function. Clipping is applied to ensure this.</li>
      <li>:cv-max The maximum output value of the handler function. Clipping is applied to ensure this.</li>
      <li>:channel Optional number of the MIDI channel to which the controller events must belong. By default
	  the channel number is ignored.</li>
    </li>
  </ul>
  The returned handler function is a property list with the following keys:
  <ul>
      <li>:update A function that is to be called with a list of midi-events.</li>
      <li>:get-output A function that returns the current output value.</li>
  </ul>
  Example:
  <pre><code>
  (cl-synthesizer-midi:relative-cc-handler
      cl-synthesizer-vendor:*arturia-minilab-mk2*
      (list (list :controller-id :ENCODER-1 :delta-percent 0.005)
            (list :controller-id :ENCODER-9 :delta-percent 0.02))
      :channel 1
      :cv-initial 2.5
      :cv-min 0
      :cv-max 5)
  </code></pre>"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (if (< cv-max cv-min)
      (cl-synthesizer:signal-assembly-error
       :format-control "cv-min greater than cv-max ~a ~a"
       :format-arguments (list cv-min cv-max)))
  (if (or (< cv-max cv-initial) (> cv-min cv-initial))
      (cl-synthesizer:signal-assembly-error
       :format-control "Initial value ~a not in CV range ~a - ~a"
       :format-arguments (list cv-initial cv-min cv-max)))
  (let ((controller-handlers nil) (cur-cv cv-initial) (cv-range (abs (- cv-max cv-min))))
    (dolist (controller controllers)
      (let ((ctrl-id (funcall (getf midi-controller :get-controller-number) (getf controller :controller-id)))
	    (delta (* cv-range (getf controller :delta-percent)))
	    (turn-speed (if (getf controller :turn-speed) (getf controller :turn-speed) (lambda (offs) offs))))
	(push
	 (lambda (midi-event-controller-id midi-event-controller-offset)
	   (if (= midi-event-controller-id ctrl-id)
	       (let ((speed
		      (*
		       (signum midi-event-controller-offset)
		       (funcall turn-speed (abs midi-event-controller-offset)))))
		 (* delta speed))
	       0))
	 controller-handlers)))
    (list
     :update (lambda (midi-events)
	       (let ((delta 0))
		 (dolist (midi-event midi-events)
		   (if (and
			(cl-synthesizer-midi-event:control-change-eventp midi-event)
			(or (not channel)
			    (= channel (cl-synthesizer-midi-event:get-channel midi-event))))
		       (let ((controller-number (cl-synthesizer-midi-event:get-controller-number midi-event))
			     (offset
			      (funcall
			       (getf midi-controller :get-controller-value-offset)
			       (cl-synthesizer-midi-event:get-controller-value midi-event))))
			 (dolist (controller-handler controller-handlers)
			   (setf delta (+ delta (funcall controller-handler controller-number offset)))))))
		 (let ((tmp (+ cur-cv delta)))
		   ;; apply clipping
		   (if (> cv-min tmp)
		       (setf cur-cv cv-min)
		       (if (< cv-max tmp)
			   (setf cur-cv cv-max)
			   (setf cur-cv tmp))))))
     :get-output (lambda () cur-cv))))
