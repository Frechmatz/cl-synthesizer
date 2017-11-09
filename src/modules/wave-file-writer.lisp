;;
;;
;; A Wave-File-Writer module
;;
;;
(in-package :cl-synthesizer-modules-wave-file-writer)

(defun wave-writer-float-to-int16 (value)
  (if (> value 1.0)
      (break))
  (if (< value -1.0)
      (break))
  (let ((i (round (* 32000 value))))
    i))

(defun input-to-wave (f)
  (wave-writer-float-to-int16
   ;; convert to -1.0 ... +1.0
   (/ f cl-synthesizer-modules-constants:+V-PEAK+)))

(defmacro n-channel-wave-file-writer (name channel-count)
  "Generates a factory function for a multiple channel wave-file-writer module. 
   name: Name of the module (name of the generated function).
   channel-count: Number of channels.
   The generated module has n input sockets channel-1...channel-n and n pass-through 
   output sockets out-1...out-n, where n is the channel count.
   The generated module factory function has the following parameters:
   - environment: Environment that specifies sample rate etc.
   - &key filename: Pathname of the wave file."
  (let ((output-name "out") (input-name "channel"))
    `(defun ,(cl-synthesizer-modules-macro-util::make-package-symbol name nil) (environment &key filename &allow-other-keys)
       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
       (let ((frames nil)
	     (inputs (cl-synthesizer-modules-macro-util::make-keyword-list ,input-name ,channel-count))
	     (outputs (cl-synthesizer-modules-macro-util::make-keyword-list ,output-name ,channel-count))
	     ,@(cl-synthesizer-modules-macro-util::make-let-list output-name channel-count))
	 (list
	  :inputs (lambda () inputs)
	  :outputs (lambda () outputs)
	  :get-output (lambda (output)
			;; TODO: Realize with cond. Did not get this to work :(
			(block nil
			  ;; generate if-clauses
			  ,@(let ((c nil))
				 (dotimes (o channel-count)
				   (push `(if (eq ,(cl-synthesizer-modules-macro-util::make-keyword output-name o) output)
					      (return ,(cl-synthesizer-modules-macro-util::make-package-symbol output-name o))) c))
				 c)
			  (error (format nil "Unknown output ~a requested from channel-wave-file-writer" output))))
	  :update (lambda (&key ,@(cl-synthesizer-modules-macro-util::make-param-list input-name channel-count))
		    ;; validate inputs
		    ,@(let ((c nil))
			   (dotimes (i channel-count)
			     (push `(if (not ,(cl-synthesizer-modules-macro-util::make-package-symbol input-name i))
					(error (format nil "Channel ~a must not be nil"
						       ,(cl-synthesizer-modules-macro-util::get-socket-number i)))) c))
			   c)
		    ;; update
		    ,@(let ((c nil))
			   (dotimes (i channel-count)
			     (push `(push (input-to-wave
					   ,(cl-synthesizer-modules-macro-util::make-package-symbol input-name i)) frames) c)
			     (push `(setf ,(cl-synthesizer-modules-macro-util::make-package-symbol output-name i)
					  ,(cl-synthesizer-modules-macro-util::make-package-symbol input-name i)) c))
			   c)
		    nil)
	  :shutdown (lambda ()
		      (let ((wave (cl-wave:open-wave filename :direction :output)))
			(cl-wave:set-num-channels wave ,channel-count)
			(cl-wave:set-sample-rate wave (getf environment :sample-rate))
			(cl-wave:set-frames wave (nreverse frames))
			(cl-wave:close-wave wave)
			(setf frames nil)))
	  )))))

(n-channel-wave-file-writer "one-channel-wave-file-writer" 1)
(n-channel-wave-file-writer "two-channel-wave-file-writer" 2)
(n-channel-wave-file-writer "three-channel-wave-file-writer" 3)
(n-channel-wave-file-writer "four-channel-wave-file-writer" 4)

