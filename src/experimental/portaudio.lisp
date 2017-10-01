(in-package :cl-playportaudio)



(defconstant +frames-per-buffer+ 1024)
(defconstant +sample-rate+ 44100d0)
(defconstant +seconds+ 15)
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 2)

(defun test-read-write-converted-echo ()
  "Record input into an array; Separate array to channels; Merge channels into array; Play last array." 
  (portaudio:with-audio
    (format t "~%=== Wire on. Will run ~D seconds . ===~%" +seconds+) 
    (portaudio:with-default-audio-stream (astream +num-channels+ +num-channels+
					:sample-format +sample-format+
					:sample-rate +sample-rate+
					:frames-per-buffer +frames-per-buffer+) 
      (dotimes (i (round (/ (* +seconds+ +sample-rate+) +frames-per-buffer+)))
	(portaudio:write-stream astream
		      (portaudio:merge-channels-into-array
		       astream
		       (portaudio:separate-array-to-channels
			astream
			(portaudio:read-stream astream)))))
      )))

;; (test-read-write-converted-echo)


