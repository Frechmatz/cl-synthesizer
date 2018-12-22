(defpackage :cl-synthesizer-playground-profiling
  (:use :cl))

(in-package :cl-synthesizer-playground-profiling)

(defun make-module (name environment)
  (declare (ignore name))
  (let ((rack (cl-synthesizer:make-rack :environment environment)))
    
    (cl-synthesizer:add-module
     rack "ADDER"
     #'cl-synthesizer-modules-adder:make-module
     :input-count 2)

    (cl-synthesizer:add-module
     rack "FIXED-OUTPUT-1"
     #'cl-synthesizer-modules-fixed-output:make-module
     :value 3.0
     :output-socket :fixed)

    (cl-synthesizer:add-module
     rack "FIXED-OUTPUT-2"
     #'cl-synthesizer-modules-fixed-output:make-module
     :value 3.0
     :output-socket :fixed)

    (cl-synthesizer:add-patch rack "FIXED-OUTPUT-1" :fixed "ADDER" :input-1)
    (cl-synthesizer:add-patch rack "FIXED-OUTPUT-2" :fixed "ADDER" :input-2)
    
    rack))

(defun make-test-rack ()
  "Test rack"
  (let ((rack (cl-synthesizer:make-rack
	       :environment (cl-synthesizer:make-environment))))
    (dotimes (i 2)
      (cl-synthesizer:add-module rack (format nil "MODULE-~a" i) #'make-module))
    rack))

(require :sb-sprof)
(defun run-rack ()
  (sb-profile:profile "CL-SYNTHESIZER")
  (cl-synthesizer:play-rack (make-test-rack) 10)
  (sb-profile:report))

;;(run-rack)


#| Profiling report: Rack with two modules and play time duration of 10 seconds
ohne optimierung
================================================================================
  seconds  |     gc     |    consed   |    calls   |  sec/call  |  name  
--------------------------------------------------------------
     2.151 |      0.036 | 112,896,928 |          1 |   2.150658 | CL-SYNTHESIZER:PLAY-RACK
     0.581 |      0.000 |           0 |  9,702,004 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-SOCKETS
     0.569 |      0.000 |           0 | 11,025,000 |   0.000000 | CL-SYNTHESIZER::SET-RACK-MODULE-STATE
     0.349 |      0.000 |           0 |  3,528,004 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-PATCH
     0.348 |      0.000 |           0 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-UPDATE-FN
     0.303 |      0.000 |           0 |  7,938,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-STATE
     0.163 |      0.000 |      32,768 |  1,323,000 |   0.000000 | CL-SYNTHESIZER::SET-STATE
     0.103 |      0.000 |           0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-FN
     0.099 |      0.000 |           0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-SOCKET
     0.032 |      0.000 |           0 |  3,528,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-MODULE
     0.002 |      0.000 |     870,624 |         14 |   0.000134 | CL-SYNTHESIZER:ADD-MODULE
     0.001 |      0.000 |     532,640 |          8 |   0.000125 | CL-SYNTHESIZER::MAKE-RACK-MODULE-PATCH
     0.001 |      0.000 |           0 |         14 |   0.000071 | CL-SYNTHESIZER::GET-RACK-MODULE-MODULE
     0.000 |      0.000 |           0 |         28 |   0.000000 | CL-SYNTHESIZER::GET-RM-MODULE
     0.000 |      0.000 |           0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-MIDI-HANDLERS
     0.000 |      0.000 |           0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-PATCH
     0.000 |      0.000 |           0 |         50 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-NAME
     0.000 |      0.000 |           0 |         14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-SHUTDOWN-FN
     0.000 |      0.000 |           0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-SOCKETS
     0.000 |      0.000 |           0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-AUDIO-HANDLERS
     0.000 |      0.000 |           0 |          3 |   0.000000 | CL-SYNTHESIZER:MAKE-RACK
     0.000 |      0.000 |           0 |          1 |   0.000000 | CL-SYNTHESIZER:MAKE-ENVIRONMENT
     0.000 |      0.000 |           0 |          4 |   0.000000 | CL-SYNTHESIZER:ADD-PATCH
--------------------------------------------------------------
     4.701 |      0.036 | 114,332,960 | 45,423,155 |            | Total

estimated total profiling overhead: 51.42 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.132e-6s total profiling, 4.7400002e-7s internal profiling
|#

#| Profiling report: Rack with two modules and play time duration of 10 seconds
mit optimierung
  seconds  |     gc     |    consed   |    calls   |  sec/call  |  name  
--------------------------------------------------------------
     2.827 |      0.034 | 112,899,360 |          1 |   2.826988 | CL-SYNTHESIZER:PLAY-RACK
     0.550 |      0.000 |           0 |  9,702,018 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-SOCKETS
     0.297 |      0.000 |           0 |  3,528,004 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-PATCH
     0.257 |      0.000 |           0 | 11,025,000 |   0.000000 | CL-SYNTHESIZER::SET-RACK-MODULE-STATE
     0.243 |      0.000 |           0 |  7,938,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-STATE
     0.210 |      0.000 |           0 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-UPDATE-FN
     0.184 |      0.000 |         320 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-ARGUMENT-LIST-PROTOTYPE
     0.099 |      0.000 |      57,104 |  1,323,000 |   0.000000 | CL-SYNTHESIZER::SET-STATE
     0.098 |      0.000 |           0 |  3,528,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-MODULE
     0.060 |      0.000 |           0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-FN
     0.016 |      0.000 |           0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-SOCKET
     0.003 |      0.000 |     848,384 |         14 |   0.000205 | CL-SYNTHESIZER:ADD-MODULE
     0.002 |      0.000 |     544,496 |          8 |   0.000250 | CL-SYNTHESIZER::MAKE-RACK-MODULE-PATCH
     0.000 |      0.000 |      32,752 |         28 |   0.000000 | CL-SYNTHESIZER::GET-RM-MODULE
     0.000 |      0.000 |           0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-MIDI-HANDLERS
     0.000 |      0.000 |           0 |         14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-MODULE
     0.000 |      0.000 |           0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-PATCH
     0.000 |      0.000 |           0 |         50 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-NAME
     0.000 |      0.000 |           0 |         14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-SHUTDOWN-FN
     0.000 |      0.000 |           0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-SOCKETS
     0.000 |      0.000 |           0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-AUDIO-HANDLERS
     0.000 |      0.000 |           0 |          3 |   0.000000 | CL-SYNTHESIZER:MAKE-RACK
     0.000 |      0.000 |           0 |          1 |   0.000000 | CL-SYNTHESIZER:MAKE-ENVIRONMENT
     0.000 |      0.000 |           0 |          4 |   0.000000 | CL-SYNTHESIZER:ADD-PATCH
--------------------------------------------------------------
     4.846 |      0.034 | 114,382,416 | 50,274,169 |            | Total

estimated total profiling overhead: 57.11 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.1359999e-6s total profiling, 4.9e-7s internal profiling

|#



#| Profiling report: Rack with zero modules and play time duration of 10 seconds
================================================================================
  seconds  |     gc     | consed |   calls   |  sec/call  |  name  
--------------------------------------------------------
     0.165 |      0.000 | 32,592 |         1 |   0.165388 | CL-SYNTHESIZER:PLAY-RACK
     0.064 |      0.000 |      0 |   882,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-SOCKETS
     0.055 |      0.000 |      0 |   441,000 |   0.000000 | CL-SYNTHESIZER::SET-STATE
     0.027 |      0.000 |      0 |   882,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-STATE
     0.020 |      0.000 |      0 |   441,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-UPDATE-FN
     0.020 |      0.000 |      0 | 1,323,000 |   0.000000 | CL-SYNTHESIZER::SET-RACK-MODULE-STATE
     0.000 |      0.000 |      0 |         4 |   0.000000 | CL-SYNTHESIZER::GET-RM-MODULE
     0.000 |      0.000 |      0 |         1 |   0.000000 | CL-SYNTHESIZER::MAKE-MIDI-HANDLERS
     0.000 |      0.000 |      0 |         2 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-MODULE
     0.000 |      0.000 |      0 |         3 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-NAME
     0.000 |      0.000 |      0 |         2 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-SHUTDOWN-FN
     0.000 |      0.000 |      0 |         1 |   0.000000 | CL-SYNTHESIZER::MAKE-AUDIO-HANDLERS
     0.000 |      0.000 |      0 |         1 |   0.000000 | CL-SYNTHESIZER:MAKE-RACK
     0.000 |      0.000 | 32,768 |         2 |   0.000000 | CL-SYNTHESIZER:ADD-MODULE
     0.000 |      0.000 |      0 |         1 |   0.000000 | CL-SYNTHESIZER:MAKE-ENVIRONMENT
--------------------------------------------------------
     0.350 |      0.000 | 65,360 | 3,969,018 |            | Total

estimated total profiling overhead: 4.88 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.2299998e-6s total profiling, 5.4000003e-7s internal profiling

|#

