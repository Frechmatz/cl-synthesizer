;;
;;
;; https://github.com/byulparan/CoreMIDI
;; vmpk-settings (Virtual Midi Piano Keyboard): MIDI-Out-Driver: CoreMidi
;; MacOs: In Audio-MIDI-Setup activate the device (MIDI-Studio -> IAC-Driver)
;; (see also http://donyaquick.com/working-with-midi-on-mac-os-x/)
;;
;;

#|
[NOTE-ON] Channel: 1  Notenum: 61  Velocity: 15
[NOTE-OFF] Channel: 1  Notenum: 61  Velocity: 0
[NOTE-ON] Channel: 1  Notenum: 61  Velocity: 15
[NOTE-OFF] Channel: 1  Notenum: 61  Velocity: 0
[CC] Channel: 1  Control: 1  Value: 98
[CC] Channel: 1  Control: 1  Value: 95

|#

(defpackage :coremidi-example
  (:use :cl))

(in-package :coremidi-example)

(ql:quickload "coremidi")

(defparameter *source* 1)

(defun test ()
  (midi:initialize)
  ;; :note
  (midi:set-midi-callback
   (midi:get-source *source*)
   :cc
   (lambda (chan control value)
     ;; [CC] Channel: 1  Control: 1  Value: 98 (Value = 0..127)
     (format t "[CC      ] Channel: ~d  Control: ~d  Value: ~d~%" chan control value)))

  (midi:set-midi-callback
   (midi:get-source *source*)
   :note-on
   (lambda (chan note vel)
     ;; [NOTE-ON] Channel: 1  Notenum: 61  Velocity: 15
     (format t "[NOTE-ON ] Channel: ~d  Notenum: ~d  Velocity: ~d~%" chan note vel)))

  (midi:set-midi-callback
   (midi:get-source *source*)
   :note-off
   (lambda (chan note vel)
     ;; [NOTE-OFF] Channel: 1  Notenum: 61  Velocity: 0
     (format t "[NOTE-OFF] Channel: ~d  Notenum: ~d  Velocity: ~d~%" chan note vel)))
  
  )

;; (midi:list-of-sources)

;; (test)
