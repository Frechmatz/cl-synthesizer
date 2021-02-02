(in-package :cl-synthesizer-test)

(define-test test-midi-relative-cc-interface-1 ()
  "Test initial output"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :mappings '((:controller-number 1 :control-value 1 :offset 1))
	      :channel nil
	      :initial-output 100)))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-relative-cc-interface-2 ()
  "Test event type filtering (must be a CC event)"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :channel nil
	      :mappings '((:controller-number 100 :control-value 112 :offset 1))
	      :initial-output 100)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-note-on-event
	       :channel 1
	       :note-number 64
	       :velocity 0))))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-relative-cc-interface-3 ()
  "Test channel filtering"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :channel 777
	      :mappings '((:controller-number 100 :control-value 200 :offset 1))
	      :initial-output 100)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :control-value 200))))
    (assert-equal 100 (get-module-output ifc :output))))

(define-test test-midi-relative-cc-interface-4 ()
  "Test multiple matching cc events"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :channel 1
	      :mappings '((:controller-number 100 :control-value 1 :offset 1)
			  (:controller-number 101 :control-value 1 :offset -10))
	      :initial-output 200)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :control-value 1)
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 999
	       :controller-number 100
	       :control-value 1)
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 101
	       :control-value 1))))
    (assert-equal 191 (get-module-output ifc :output))))

(define-test test-midi-relative-cc-interface-5 ()
  "Test clipping of initial output"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :channel nil
	      :mappings '((:controller-number 100 :control-value 11 :offset 1))
	      :initial-output 100
	      :min-output 200
	      :max-output 300)))
    (assert-equal 200 (get-module-output ifc :output))))

(define-test test-midi-relative-cc-interface-6 ()
  "Test clipping of max output"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :mappings '((:controller-number 100 :control-value 1 :offset 1000))
	      :channel nil
	      :initial-output 100
	      :min-output 200
	      :max-output 300)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :control-value 1))))
    (assert-equal 300 (get-module-output ifc :output))))

(define-test test-midi-cc-interface-7 ()
  "Test clipping of min output"
  (let ((ifc (cl-synthesizer-modules-midi-relative-cc-interface:make-module
	      "MidiCC"
	      (cl-synthesizer:make-environment)
	      :mappings '((:controller-number 100 :control-value 1 :offset -1000))
	      :channel nil
	      :initial-output 100
	      :min-output 200
	      :max-output 300)))
    (update-module ifc (list :midi-events
	     (list
	      (cl-synthesizer-midi-event:make-control-change-event
	       :channel 1
	       :controller-number 100
	       :control-value 1))))
    (assert-equal 200 (get-module-output ifc :output))))
