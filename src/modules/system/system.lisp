(in-package :cl-synthesizer-modules-system)

(defun make-module (name environment &key)
  "Creates a module which exposes runtime information of its rack.
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
    </ul></p>
    <p>The module has no inputs.</p>
    <p>The module has the following outputs:
    <ul>
       <li>:sample-rate The sample rate of the rack.</li>
       <li>:ticks The tick number that is being processed. The first tick has the number 0.</li>
       <li>:milliseconds The time in milliseconds that has passed since the first tick.</li>
       <li>:seconds The time in seconds that has passed since the first tick.</li>
    </ul></p>"
  (declare (ignore name))
  (let ((ticks -1)
	(sample-rate (getf environment :sample-rate))
	(milliseconds-dirty t) (milliseconds nil)
	(seconds-dirty t) (seconds nil))
    (labels
	((get-seconds ()
	   (if seconds-dirty
	       (progn
		 (setf seconds (/ ticks sample-rate))
		 (setf seconds-dirty nil)))
	   seconds)
	(get-milliseconds ()
	   (if milliseconds-dirty
	       (progn
		 (setf milliseconds (* 1000.0 (get-seconds)))
		 (setf milliseconds-dirty nil)))
	   milliseconds))
      (let ((outputs
	      (list
	       :ticks (list :get (lambda() ticks))
	       :sample-rate (list :get (lambda() sample-rate))
	       :milliseconds (list :get #'get-milliseconds)
	       :seconds (list :get #'get-seconds))))
	(list
	 :inputs (lambda() nil)
	 :outputs (lambda() outputs)
	 :update (lambda ()
		   (setf ticks (+ 1 ticks))
		   (setf milliseconds-dirty t)
		   (setf seconds-dirty t)))))))
