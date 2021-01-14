;;
;; Did not get this to work with Virtual Midi Piano Keyboard
;;
;; First steps (macos):
;; 1. brew install portaudio
;; 2. patch cl-portaudio/setup.lisp: add "libportmidi.dylib"
;; 3. http://donyaquick.com/working-with-midi-on-mac-os-x/
;; 3.1. add port if not present
;; 3.2. activate device (otherwise devices won't be seen by portaudio)
;;
;;

(defpackage :portmidi-example
  (:use :cl))

(in-package :portmidi-example)

(ql:quickload "portmidi")


(defun play-notes ()
  (pm:initialize)
  (let ((output-device-id 3)) ;;(pm:get-default-output-device-id)))
    (let ((midi-out (pm:open-output output-device-id 1024 0)))
      (pm:write-short-midi midi-out 0 (pm:note-on (pm:channel 0) 80))
      (pm:write-short-midi midi-out 0 (pm:note-off (pm:channel 0) 80))
      (pm:close-midi midi-out)))
  (pm:terminate)
  (format t "~%DONE"))
    
;; (play-notes)


(defun read-events ()
  (pm:initialize)
  (let ((input-device-id 1)) ;;(pm:get-default-input-device-id)))
    (format t "~%Input device-id is ~a" input-device-id)
    (let ((midi-input-stream (pm:open-input input-device-id 1024)))
      (format t "~%Input stream is ~a" midi-input-stream)
      (dotimes (i 1)
	(format t "~%Reading event...")
	(let ((midi-event (pm:read-midi midi-input-stream)))
	  (format t "~%Received event: ~a~%" midi-event)))
      (format t "~%Closing input stream")
      (pm:close-midi midi-input-stream)
      (pm:terminate)
      (format t "~%DONE"))))

  
;; (read-events)
  
