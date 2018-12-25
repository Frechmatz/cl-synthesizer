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
initial not optimized version
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
First step: Minimize consing
  seconds  |     gc     |   consed   |    calls   |  sec/call  |  name  
-------------------------------------------------------------
     0.984 |      0.000 |     15,840 |          1 |   0.983604 | CL-SYNTHESIZER:PLAY-RACK
     0.361 |      0.000 |          0 |  9,702,018 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-SOCKETS
     0.138 |      0.000 |     52,448 |  1,323,000 |   0.000000 | CL-SYNTHESIZER::SET-STATE
     0.108 |      0.030 | 56,438,432 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-ARGUMENT-LIST-PROTOTYPE
     0.106 |      0.000 |          0 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-UPDATE-FN
     0.063 |      0.000 |          0 |  3,528,004 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-PATCH
     0.049 |      0.000 |          0 | 11,025,000 |   0.000000 | CL-SYNTHESIZER::SET-RACK-MODULE-STATE
     0.025 |      0.000 |          0 |  7,938,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-STATE
     0.013 |      0.000 |          0 |  3,528,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-MODULE
     0.005 |      0.000 |          0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-FN
     0.004 |      0.000 |    894,432 |         14 |   0.000276 | CL-SYNTHESIZER:ADD-MODULE
     0.002 |      0.000 |    562,224 |          8 |   0.000249 | CL-SYNTHESIZER::MAKE-RACK-MODULE-PATCH
     0.001 |      0.000 |          0 |         14 |   0.000071 | CL-SYNTHESIZER::GET-RACK-MODULE-SHUTDOWN-FN
     0.000 |      0.000 |          0 |         28 |   0.000000 | CL-SYNTHESIZER::GET-RM-MODULE
     0.000 |      0.000 |          0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-SOCKET
     0.000 |      0.000 |          0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-MIDI-HANDLERS
     0.000 |      0.000 |          0 |         14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-MODULE
     0.000 |      0.000 |          0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-PATCH
     0.000 |      0.000 |          0 |         50 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-NAME
     0.000 |      0.000 |          0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-SOCKETS
     0.000 |      0.000 |          0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-AUDIO-HANDLERS
     0.000 |      0.000 |          0 |          3 |   0.000000 | CL-SYNTHESIZER:MAKE-RACK
     0.000 |      0.000 |          0 |          1 |   0.000000 | CL-SYNTHESIZER:MAKE-ENVIRONMENT
     0.000 |      0.000 |          0 |          4 |   0.000000 | CL-SYNTHESIZER:ADD-PATCH
-------------------------------------------------------------
     1.859 |      0.030 | 57,963,376 | 50,274,169 |            | Total

estimated total profiling overhead: 58.62 seconds
overhead estimation parameters:
  4.0000003e-9s/call, 1.166e-6s total profiling, 5.04e-7s internal profiling

|#

#| Second step: with minimized consing. Approach holds a "static" list
   of input arguments for update function, which is modified during successive
   update calls.
   
measuring PROFILE overhead..done
  seconds  |     gc     |   consed  |    calls   |  sec/call  |  name  
------------------------------------------------------------
     3.325 |      0.000 |         0 |          1 |   3.324627 | CL-SYNTHESIZER:PLAY-RACK
     0.306 |      0.000 |         0 |  9,702,018 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-SOCKETS
     0.152 |      0.000 |         0 |  3,528,004 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-PATCH
     0.100 |      0.000 |         0 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-UPDATE-FN
     0.072 |      0.000 |         0 |  4,851,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-INPUT-ARGUMENT-LIST-PROTOTYPE
     0.060 |      0.000 |         0 |  1,323,000 |   0.000000 | CL-SYNTHESIZER::SET-STATE
     0.043 |      0.000 |         0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-FN
     0.013 |      0.000 |         0 |  1,764,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-SOCKET
     0.003 |      0.000 |   897,840 |         14 |   0.000205 | CL-SYNTHESIZER:ADD-MODULE
     0.001 |      0.000 |   558,800 |          8 |   0.000124 | CL-SYNTHESIZER::MAKE-RACK-MODULE-PATCH
     0.001 |      0.000 |         0 |         28 |   0.000034 | CL-SYNTHESIZER::GET-RM-MODULE
     0.000 |      0.000 |         0 |  7,938,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-STATE
     0.000 |      0.000 |         0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-MIDI-HANDLERS
     0.000 |      0.000 |         0 |  3,528,000 |   0.000000 | CL-SYNTHESIZER::GET-RACK-PATCH-MODULE
     0.000 |      0.000 |         0 |         14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-MODULE
     0.000 |      0.000 |         0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-PATCH
     0.000 |      0.000 |         0 |         50 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-NAME
     0.000 |      0.000 |         0 | 11,025,000 |   0.000000 | CL-SYNTHESIZER::SET-RACK-MODULE-STATE
     0.000 |      0.000 |         0 |         14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-SHUTDOWN-FN
     0.000 |      0.000 |         0 |          4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-SOCKETS
     0.000 |      0.000 |         0 |          1 |   0.000000 | CL-SYNTHESIZER::MAKE-AUDIO-HANDLERS
     0.000 |      0.000 |         0 |          3 |   0.000000 | CL-SYNTHESIZER:MAKE-RACK
     0.000 |      0.000 |         0 |          1 |   0.000000 | CL-SYNTHESIZER:MAKE-ENVIRONMENT
     0.000 |      0.000 |         0 |          4 |   0.000000 | CL-SYNTHESIZER:ADD-PATCH
------------------------------------------------------------
     4.077 |      0.000 | 1,456,640 | 50,274,169 |            | Total

estimated total profiling overhead: 58.22 seconds
overhead estimation parameters:
  4.0000003e-9s/call, 1.158e-6s total profiling, 5.18e-7s internal profiling

|#

#| Third step with inlining

measuring PROFILE overhead..done
  seconds  |     gc     |   consed  |   calls   |  sec/call  |  name  
-----------------------------------------------------------
     0.828 |      0.000 |         0 |         1 |   0.828269 | CL-SYNTHESIZER:PLAY-RACK
     0.061 |      0.000 |    52,448 | 1,323,000 |   0.000000 | CL-SYNTHESIZER::SET-STATE
     0.004 |      0.000 |   848,128 |        14 |   0.000278 | CL-SYNTHESIZER:ADD-MODULE
     0.001 |      0.000 |   550,016 |         8 |   0.000125 | CL-SYNTHESIZER::MAKE-RACK-MODULE-PATCH
     0.000 |      0.000 |         0 |        28 |   0.000000 | CL-SYNTHESIZER::GET-RM-MODULE
     0.000 |      0.000 |         0 |         1 |   0.000000 | CL-SYNTHESIZER::MAKE-MIDI-HANDLERS
     0.000 |      0.000 |         0 |        14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-MODULE
     0.000 |      0.000 |         0 |         4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-PATCH
     0.000 |      0.000 |         0 |        50 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-NAME
     0.000 |      0.000 |         0 |        14 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-SHUTDOWN-FN
     0.000 |      0.000 |         0 |         4 |   0.000000 | CL-SYNTHESIZER::GET-RACK-MODULE-OUTPUT-SOCKETS
     0.000 |      0.000 |         0 |         1 |   0.000000 | CL-SYNTHESIZER::MAKE-AUDIO-HANDLERS
     0.000 |      0.000 |         0 |         3 |   0.000000 | CL-SYNTHESIZER:MAKE-RACK
     0.000 |      0.000 |         0 |         1 |   0.000000 | CL-SYNTHESIZER:MAKE-ENVIRONMENT
     0.000 |      0.000 |         0 |         4 |   0.000000 | CL-SYNTHESIZER:ADD-PATCH
-----------------------------------------------------------
     0.894 |      0.000 | 1,450,592 | 1,323,147 |            | Total

estimated total profiling overhead: 1.51 seconds
overhead estimation parameters:
  6.0e-9s/call, 1.14e-6s total profiling, 5.0e-7s internal profiling

|#
