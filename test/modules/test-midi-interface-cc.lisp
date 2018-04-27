(in-package :cl-synthesizer-test)


(define-test test-midi-interface-cc-output-not-available ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer::make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :CV-1 nil)))))

(define-test test-midi-interface-cc-output-not-keyword ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer::make-environment)
		:voice-count 1
		:controller-handler (list
				     (list "CV-1" nil)))))

(define-test test-midi-interface-cc-invalid-handler-1 ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer::make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :my-controller nil)))))

;; get-output function missing in handler
(define-test test-midi-interface-cc-invalid-handler-2 ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer::make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :my-controller (list :update (lambda () nil)))))))

;; update function missing in handler
(define-test test-midi-interface-cc-invalid-handler-3 ()
	     (expect-assembly-exception
	       (cl-synthesizer-modules-midi-interface:midi-interface
		"Test-Midi-Interface"
		(cl-synthesizer::make-environment)
		:voice-count 1
		:controller-handler (list
				     (list :my-controller (list :get-output (lambda () nil)))))))


(define-test test-midi-interface-cc-1 ()
	     (let ((ifc
		    (cl-synthesizer-modules-midi-interface:midi-interface
		     "Test-Midi-Interface"
		     (cl-synthesizer::make-environment)
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
		     (cl-synthesizer::make-environment)
		     :voice-count 1
		     :controller-handler (list (list
						:my-controller
						(list
						 :get-output (lambda () 99)
						 :update (lambda (midi-events) nil)))))))
	       (assert-equal 99 (funcall (getf ifc :get-output) :my-controller))))


	     
(defparameter *MIDI-IFC-TEST-VENDOR*
  (list
   :ENCODER-CONTROLLER-NUMBERS
   (list :ENCODER-1 (list :CONTROLLER-NUMBER 112)
	 :ENCODER-2 (list :CONTROLLER-NUMBER 74))
   :RELATIVE-ENCODER-OFFSET
   (lambda (controller-value)
     (cond
       ((eq 61 controller-value) -5)
       ((eq 62 controller-value) -3)
       ((eq 63 controller-value) -1)
       ((eq 65 controller-value) 1)
       ((eq 66 controller-value) 3)
       ((eq 67 controller-value) 5)
       (t 0)))))

(defparameter *MIDI-IFC-TEST-CC-HANDLER-INITIAL-CV* 50)

(define-test test-midi-interface-cc-3 ()
	     (let ((ifc
		    (cl-synthesizer-modules-midi-interface:midi-interface
		     "Test-Midi-Interface"
		     (cl-synthesizer::make-environment)
		     :voice-count 1
		     :controller-handler (list (list
						:my-controller
						(cl-synthesizer-vendor-cc-handler:7-bit-relative
							 *MIDI-IFC-TEST-VENDOR*
							 :ENCODER-2
							 :cv-initial *MIDI-IFC-TEST-CC-HANDLER-INITIAL-CV*
							 :cv-min 0
							 :cv-max 1270))))))
	       (funcall (getf ifc :update)
			:midi-events
			(list
			 (cl-synthesizer-midi-event:make-control-change-event
			  1
			  (cl-synthesizer-vendor:get-controller-number *MIDI-IFC-TEST-VENDOR* :ENCODER-2)
			  65)))
	       (assert-equal (+ *MIDI-IFC-TEST-CC-HANDLER-INITIAL-CV* 10)
			     (funcall (getf ifc :get-output) :my-controller))))



		     
