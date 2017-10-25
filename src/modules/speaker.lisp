
(in-package :cl-synthesizer-modules-speaker)

(defmacro n-channel-speaker (name channel-count)
  "The generated module factory function has the following parameters:
   - environment: Environment that specifies sample rate etc.
   - &key driver: Driver to be used, for example \"coreaudio\"."
  (let ((input-name "channel") (buf-length-seconds 1))
    `(defun ,(cl-synthesizer-modules-macro-util::make-package-symbol name nil) (environment &key driver &allow-other-keys)
       (declare (optimize (debug 3) (speed 0) (space 0)))
       (let ((out nil)
	     (buffer (make-array
		      (* ,channel-count ,buf-length-seconds (getf environment :sample-rate))
		      :element-type 'single-float :adjustable nil))
	     (buffer-pos 0)
	     (inputs (cl-synthesizer-modules-macro-util::make-keyword-list ,input-name ,channel-count)))
	 (flet ((init-out ()
		  (if (not out)
		      (progn
			(setf out (make-instance 'cl-out123:output))
			(cl-out123:connect out :driver driver)
			(cl-out123:start
			 out
			 :rate (getf environment :sample-rate)
			 :channels ,channel-count
			 :encoding :float))))
		(flush-buffer (force)
		  (declare (optimize (debug 3) (speed 0) (space 0)))
		  (if (or force (>= buffer-pos (length buffer)))
		      (progn
			;;(format t "~%Flushing speaker buffer~%")
			(cl-out123:play out buffer buffer-pos)
			(setf buffer-pos 0)))))
	   (list
	    :inputs (lambda () inputs)
	    :outputs (lambda () nil)
	    :get-output (lambda (output)
			  ;; todo: signal error
			  (declare (ignore output))
			  nil)
	    :update (lambda (&key ,@(cl-synthesizer-modules-macro-util::make-param-list input-name channel-count))
		      (init-out)
		      ;; validate inputs
		      ;; todo: provide this code as macro in macro-util package
		      ,@(let ((c nil))
			     (dotimes (i channel-count)
			       (push `(if (not ,(cl-synthesizer-modules-macro-util::make-package-symbol input-name i))
					  (error (format nil "Channel ~a must not be nil"
							 ,(cl-synthesizer-modules-macro-util::get-socket-number i)))) c))
			     c)
		      (flush-buffer nil)
		      ,@(let ((c nil))
			     (dotimes (i channel-count)
			       (push `(progn
					(setf (aref buffer buffer-pos)
					      (coerce
					       (/
						;; convert to -1.0 ... +1.0
						,(cl-synthesizer-modules-macro-util::make-package-symbol input-name i)
						cl-synthesizer-modules-constants:+V-PEAK+)
					       'single-float))
					(setf buffer-pos (+ 1 buffer-pos))) c))
			     c)
		      nil)
	    :shutdown (lambda ()
			(flush-buffer t)
			(cl-out123:disconnect out)
			nil)
	    ))))))

#|
(defun test ()
  (let ((w (n-channel-speaker "speaker" 2)))
    (let ((instance (funcall w (cl-synthesizer::make-environment))))
      (format t "~%Inputs: ~a~%" (funcall (getf instance :inputs)) )
      (format t "~%Outputs: ~a~%" (funcall (getf instance :outputs)) )
      (format t "~%Output 1: ~a~%" (funcall (getf instance :get-output) :out-1) )
      (funcall (getf instance :update) :channel-1 0.5 :channel-2 0.7)
      (funcall (getf instance :shutdown))

      )))

(test)
|#

(n-channel-speaker "mono-speaker" 1)
(n-channel-speaker "stereo-speaker" 2)

