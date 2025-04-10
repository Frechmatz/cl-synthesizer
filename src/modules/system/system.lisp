(in-package :cl-synthesizer-modules-system)

(defun make-module (name environment &key)
  "Creates a module which exposes runtime information of its rack.
   <p>The function has the following parameters:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
    </ul></p>
    <p>The module has no inputs.</p>
    <p>The module has no outputs.</p>
    <p>The module exposes the following states:
    <ul>
       <li>:sample-rate The sample rate of the rack.</li>
       <li>:elapsed-ticks Number of processed ticks.</li>
       <li>:elapsed-milliseconds Elapsed time in milliseconds.</li>
       <li>:elapsed-seconds Elapsed time in seconds.</li>
    </ul></p>"
  (declare (ignore name))
  (let ((ticks 0) (sample-rate (getf environment :sample-rate)))
    (list
     :inputs (lambda() nil)
     :outputs (lambda() nil)
     :update (lambda()
	       (setf ticks (+ 1 ticks)))
     :state (lambda(key)
	      (cond
		((eq key :elapsed-ticks)
		 ticks)
		((eq key :elapsed-milliseconds)
		 (* 1000.0 (/ ticks sample-rate)))
		((eq key :elapsed-seconds)
		 (/ ticks sample-rate))
		((eq key :sample-rate)
		 sample-rate)
		(t nil))))))
