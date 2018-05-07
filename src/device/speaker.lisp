;;
;;
;; A Speaker Device based on cl-out123
;;
;;
(in-package :cl-synthesizer-device-speaker)

(alexandria:define-constant +V-PEAK+ 5.0)

(defmacro n-channel-speaker (name channel-count)
  "The generated module factory function has the following parameters:
   - environment: Environment that specifies sample rate etc.
   - &key driver: Driver to be used, for example \"coreaudio\"."
  (let ((input-name "channel") (buf-length-samples 1000))
    `(defun ,(cl-synthesizer-macro-util::make-package-symbol name nil) (name environment &key driver &allow-other-keys)
       (declare (optimize (debug 3) (speed 0) (space 0)))
       (declare (ignore name))
       (let ((out nil)
	     (buffer (make-array
		      (* ,channel-count ,buf-length-samples)
		      :element-type 'single-float :adjustable nil))
	     (buffer-pos 0))
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
	    :inputs (lambda () (cl-synthesizer-macro-util::make-keyword-list ,input-name ,channel-count))
	    :outputs (lambda () nil)
	    :update (lambda (&key ,@(cl-synthesizer-macro-util::make-param-list input-name channel-count))
		      (init-out)
		      ;; validate inputs
		      ;; todo: provide this code as macro in macro-util package
		      ,@(let ((c nil))
			     (dotimes (i channel-count)
			       (push `(if (not ,(cl-synthesizer-macro-util::make-package-symbol input-name i))
					  (error (format nil "Channel ~a must not be nil"
							 ,(cl-synthesizer-macro-util::get-socket-number i)))) c))
			     c)
		      (flush-buffer nil)
		      ,@(let ((c nil))
			     (dotimes (i channel-count)
			       (push `(progn
					(setf (aref buffer buffer-pos)
					      (coerce
					       (/
						;; convert to -1.0 ... +1.0
						,(cl-synthesizer-macro-util::make-package-symbol input-name i)
						+V-PEAK+)
					       'single-float))
					(setf buffer-pos (+ 1 buffer-pos))) c))
			     c)
		      nil)
	    :shutdown (lambda ()
			(flush-buffer t)
			(cl-out123:disconnect out)
			nil)
	    ))))))

(n-channel-speaker "mono-speaker" 1)
(n-channel-speaker "stereo-speaker" 2)


