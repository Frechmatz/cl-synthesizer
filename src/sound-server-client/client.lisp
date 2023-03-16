(in-package :cl-synthesizer-java-sound-client)

(defclass cl-synthesizer-controller (controller)
  ((cur-frame-count :initform 0)
   (max-frame-count :initform 0)
   (rack :initarg :rack)
   (output-sockets :initarg :output-sockets)
   (output-buffer :initform nil)
   (v-peak :initarg :v-peak)))

(defun done-p (controller)
  (<= (slot-value controller 'max-frame-count)
      (slot-value controller 'cur-frame-count)))

(defmethod initialize-instance :around
    ((instance cl-synthesizer-controller)
     &key
       rack
       output-sockets
       v-peak
       duration-seconds 
       sample-width)
  (let ((environment (cl-synthesizer:get-environment rack)))
    (call-next-method
     instance
     :rack rack
     :output-sockets output-sockets
     :v-peak v-peak
     :channel-count (length output-sockets)
     :duration-seconds duration-seconds
     :sample-width sample-width
     :sample-rate (truncate (getf environment :sample-rate)))))

(defmethod initialize-instance :after
    ((instance cl-synthesizer-controller)
     &key
       duration-seconds
     &allow-other-keys)
  (setf (slot-value instance 'max-frame-count)
	(* (cl-java-sound-client:get-sample-rate instance) duration-seconds))
  (setf (slot-value instance 'output-buffer)
	(make-array (cl-java-sound-client:get-channel-count instance)))
  (cl-synthesizer-monitor:add-monitor
   (slot-value instance 'rack)
   #'cl-synthesizer-monitor-buffer-agent:make-backend
   (slot-value instance 'output-sockets)
   :buffer (slot-value instance 'output-buffer)))

(defmethod notify-frames-requested ((instance cl-synthesizer-controller))
  (if (done-p instance)
      (cl-java-sound-client:close-connection instance)
      (cl-java-sound-client:frames instance)))

(defmethod render-frames ((instance cl-synthesizer-controller) frame-count sample-buffer)
  (let ((rendered-frame-count 0)
	(channel-count (get-channel-count instance))
	(tick (getf (slot-value instance 'rack) :update))
	(output-buffer (slot-value instance 'output-buffer))
	(v-peak (slot-value instance 'v-peak)))
    (dotimes (frame-number frame-count)
      (if (done-p instance)
	  (return)
	  (progn
	    (incf rendered-frame-count)
	    (incf (slot-value instance 'cur-frame-count))
	    (funcall tick)
	    (dotimes (i channel-count)
	      (setf (aref
		     sample-buffer
		     (+ i (* frame-number channel-count)))
		    (/ (elt output-buffer i) v-peak))))))
  rendered-frame-count))

