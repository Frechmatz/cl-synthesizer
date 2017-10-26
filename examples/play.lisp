(in-package :cl-synthesizer-examples)

(defun play-rack (rack duration-seconds)
  (let ((start (get-internal-real-time)))
    (dotimes (i (* duration-seconds (getf (slot-value rack 'cl-synthesizer::environment) :sample-rate)))
      (cl-synthesizer::update-rack rack))
    (let ((end (get-internal-real-time)))
      (cl-synthesizer::shutdown-rack rack)
      (format t "Elapsed time in seconds before shutdown: ~a~%" (/ (- end start) internal-time-units-per-second))))
  "DONE")

