(in-package :cl-synthesizer-test)

(define-test test-waveform-sine-1 ()
  "Test the sine transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-core:phase-generator 1000))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-core:phase-sine-converter (funcall phase-generator 1)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))


(define-test test-waveform-saw-1 ()
  "Test the saw transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-core:phase-generator 1000))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-core:phase-saw-converter (funcall phase-generator 1)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))

(define-test test-waveform-triangle-1 ()
  "Test the triangle transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-core:phase-generator 1000))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-core:phase-triangle-converter (funcall phase-generator 1)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))

(define-test test-waveform-square-1 ()
  "Test the square transfer function by counting the zero-crossings of the signal"
  (let ((trigger (funcall #'cl-synthesizer-test::zero-crossing-trigger))
	(phase-generator (cl-synthesizer-core:phase-generator 1000))
	(trigger-count 0))
    (dotimes (i 7250)
		 (if (funcall trigger (cl-synthesizer-core:phase-square-converter (funcall phase-generator 1)))
		     (setf trigger-count (+ trigger-count 1))))
    ;; 14 zero-crossings
    (assert-equal 14 trigger-count)))
