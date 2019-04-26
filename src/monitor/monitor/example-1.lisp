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
	     (let ((input-sine nil) (input-phase nil))
	       (values 
		(list
		 :inputs (lambda()
			   (list
			    :sine (lambda(value) (setf input-sine value))
			    :phase (lambda(value) (setf input-phase value))))
		 :outputs (lambda()
			    (list
			     :sine (lambda() input-sine)
			     :phase (lambda() input-phase)))
		 :update (lambda () nil))
		'(:sine :phase)
		))))
      
      (cl-synthesizer-monitor:add-monitor
       rack
       #'instantiate-handler
       '(("VCO" :output-socket :sine)
	 ("VCO" :state :phase))))
    
    rack))

(defun run-example ()
  (let ((rack (example)))
    (funcall (getf rack :update))
    (funcall (getf rack :update))))

;; (run-example)
