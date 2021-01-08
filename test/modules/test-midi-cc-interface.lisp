(in-package :cl-synthesizer-test)

(define-test test-midi-cc-interface-1 ()
  "Test initial output"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda ())
	      :channel nil
	      :initial-output 100)))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-2 ()
  "Test event type filtering (must be a CC event)"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda ())
	      :channel nil
	      :initial-output 100)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-note-on-event
	       :channel 1
	       :note-number 64
	       :velocity 0))))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-3 ()
  "Test controller number filtering"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda(o n v) nil)
	      :channel nil
	      :initial-output 100)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 101
	       :controller-value 200))))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-4 ()
  "Test channel filtering"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda(o n v) nil)
	      :channel 777
	      :initial-output 100)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :controller-value 200))))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-5 ()
  "Test arguments of transform function. Test return value of transform function"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler
	      (lambda (o n v)
		(assert-equal o 200)
		(assert-equal n 100)
		(assert-equal v 999)
		v)
	      :channel nil
	      :initial-output 200)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :controller-value 999))))
    (assert-equal 999 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-6 ()
  "Test multiple matching cc events"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100 101)
	      :transform-handler
	      (lambda (o n v)
		(cond
		  ((= n 100)
		   (+ o v))
		  ((= n 101)
		   (+ o (* v 2)))
		  (t
		   (assert-equal 1 2))))
	      :channel 1
	      :initial-output 200)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :controller-value 500)
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 999
	       :controller-number 100
	       :controller-value 500)
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 101
	       :controller-value 1000))))
    (assert-equal 2700 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-7 ()
  "Test clipping of initial output"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda ())
	      :channel nil
	      :initial-output 100
	      :min-output 200
	      :max-output 300)))
    (assert-equal 200 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-8 ()
  "Test clipping of max output"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda (o n v) v)
	      :channel nil
	      :initial-output 100
	      :min-output 200
	      :max-output 300)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :controller-value 500))))
    (assert-equal 300 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-9 ()
  "Test clipping of min output"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda (o n v) v)
	      :channel nil
	      :initial-output 100
	      :min-output 200
	      :max-output 300)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :controller-value 50))))
    (assert-equal 200 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-10 ()
  "Test clipping of min output with floating point values"
  (let ((ifc (cl-synthesizer-modules-midi-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :controller-numbers '(100)
	      :transform-handler (lambda (o n v) v)
	      :channel nil
	      :initial-output 100.0
	      :min-output 200.5
	      :max-output 300.5)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :controller-value 50))))
    (assert-equal 200.5 (get-module-output ifc :output))))

