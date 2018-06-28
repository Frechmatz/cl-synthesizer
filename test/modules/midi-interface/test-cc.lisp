(in-package :cl-synthesizer-test)


(define-test test-midi-interface-cc-output-not-available ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer:make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :CV-1 nil)))))

(define-test test-midi-interface-cc-output-not-keyword ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer:make-environment)
		:voice-count 1
		:controller-handler (list
				     (list "CV-1" nil)))))

(define-test test-midi-interface-cc-invalid-handler-1 ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer:make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :my-controller nil)))))

;; get-output function missing in handler
(define-test test-midi-interface-cc-invalid-handler-2 ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer:make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :my-controller (list :update (lambda () nil)))))))

;; update function missing in handler
(define-test test-midi-interface-cc-invalid-handler-3 ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer:make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :my-controller (list :get-output (lambda () nil)))))))


(define-test test-midi-interface-cc-1 ()
	     (let ((ifc
		    (cl-synthesizer-modules-midi-interface:midi-interface
		     "Test-Midi-Interface"
		     (cl-synthesizer:make-environment)
		     :voice-count 1
		     :controller-handler (list (list
						:my-controller
						(list
						 :get-output (lambda () 99)
						 :update (lambda (midi-events) nil)))))))
	       (assert-equal 99 (funcall (getf ifc :get-output) :my-controller))))

(define-test test-midi-interface-cc-2 ()
	     (let ((ifc
		    (cl-synthesizer-modules-midi-interface:midi-interface
		     "Test-Midi-Interface"
		     (cl-synthesizer:make-environment)
		     :voice-count 1
		     :controller-handler (list (list
						:my-controller
						(list
						 :get-output (lambda () 99)
						 :update (lambda (midi-events) nil)))))))
	       (assert-equal 99 (funcall (getf ifc :get-output) :my-controller))))
	     
(defparameter *MIDI-IFC-TEST-VENDOR*
    (list
     :get-controller-number
     (lambda (id)
       (getf '(:ENCODER-1 112 :ENCODER-2 74) id))
     :get-controller-value-offset
     (lambda (controller-value)
     (cond
       ((= 61 controller-value) -5)
       ((= 62 controller-value) -3)
       ((= 63 controller-value) -1)
       ((= 65 controller-value) 1)
       ((= 66 controller-value) 3)
       ((= 67 controller-value) 5)
       (t 0)))))

(defparameter *MIDI-IFC-TEST-CC-HANDLER-INITIAL-CV* 50.0)

(define-test test-midi-interface-cc-3 ()
  (let ((ifc
	 (cl-synthesizer-modules-midi-interface:midi-interface
	  "Test-Midi-Interface"
	  (cl-synthesizer:make-environment)
	  :voice-count 1
	  :controller-handler (list (list
				     :my-controller
				     (cl-synthesizer-midi:relative-cc-handler
				      *MIDI-IFC-TEST-VENDOR*
				      (list (list :controller-id :ENCODER-2 :delta-percent 0.1))
				      :cv-initial *MIDI-IFC-TEST-CC-HANDLER-INITIAL-CV*
				      :cv-min 0
				      :cv-max 100))))))
    (funcall (getf ifc :update)
	     :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       1
	       (funcall (getf *MIDI-IFC-TEST-VENDOR* :get-controller-number) :ENCODER-2)
	       65)))
    (assert-equal (+ *MIDI-IFC-TEST-CC-HANDLER-INITIAL-CV* 10)
		  (funcall (getf ifc :get-output) :my-controller))))



		     
