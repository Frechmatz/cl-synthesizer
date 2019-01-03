(in-package :cl-synthesizer-core)

(defun round-time (time sample-rate)
  "Rounds a time value by factoring in the time resolution of the sample rate.</br>
   Examples:</br>
   (round-time 10.000005 44100) => 10.0</br>
   (round-time 10.00005 44100)  => 10.0000454</br>
   (round-time -10.000005 44100) => -10.0</br>
   (round-time -10.00005 44100) => -10.0000454"
  (declare (type single-float time sample-rate))
  (/ (round time (/ 1.0 sample-rate)) sample-rate))

