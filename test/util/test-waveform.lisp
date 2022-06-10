(in-package :cl-synthesizer-test)

(define-test test-waveform-sine-1 ()
  "Test the sine transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-util:phase-sine-converter (funcall phase-generator 1.0)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))


(define-test test-waveform-saw-1 ()
  "Test the saw transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 7250)
      (if (funcall trigger (cl-synthesizer-util:phase-saw-converter (funcall phase-generator 1.0)))
	  (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))

(define-test test-waveform-triangle-1 ()
  "Test the triangle transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-util:phase-triangle-converter (funcall phase-generator 1.0)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))

(define-test test-waveform-square-1 ()
  "Test the square transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-util:phase-square-converter (funcall phase-generator 1.0)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))

(define-test test-waveform-square-2 ()
  "Test the square transfer function by counting the zero-crossings of the signal and setting a duty-cycle"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 10000)
		 (if (funcall trigger (cl-synthesizer-util:phase-square-converter (funcall phase-generator 1.0) :duty-cycle 0.25))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 20 zero-crossings
    (assert-equal 20 trigger-count)))

(define-test test-waveform-square-3 ()
  "Test the square transfer function by counting the zero-crossings of the signal and setting a duty-cycle of 100%"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-util:phase-square-converter (funcall phase-generator 1.0) :duty-cycle 1.0))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 0 zero-crossings
    (assert-equal 0 trigger-count)))

(define-test test-waveform-square-4 ()
  "Test the square transfer function by counting the zero-crossings of the signal and setting a duty-cycle of 0%"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-util:phase-generator 1000.0))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-util:phase-square-converter (funcall phase-generator 1.0) :duty-cycle 0.0))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 0 zero-crossings
    (assert-equal 0 trigger-count)))
