;;
;;
;; A Wave-File-Writer module
;;
;;
(in-package :cl-synthesizer-device-wave-file-writer)

(alexandria:define-constant +V-PEAK+ 5.0)

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
    `(defun ,(cl-synthesizer-macro-util::make-package-symbol name nil) (name environment &key filename &allow-other-keys)
       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
       (let ((frames nil)
	     ,@(cl-synthesizer-macro-util::make-let-list output-name channel-count))
	 (labels (
		(wave-writer-float-to-int16 (value)
		  (cond
		    ((> value 1.0)
		     1)
		    ((< value -1.0)
		     -1)
		    (t
		    (round (* 32000 value)))))
		(input-to-wave (f)
		  (wave-writer-float-to-int16
		   ;; convert to -1.0 ... +1.0
		   (/ f +V-PEAK+))))
	   (list
	    :inputs (lambda () (cl-synthesizer-macro-util::make-keyword-list ,input-name ,channel-count))
	    :outputs (lambda () (cl-synthesizer-macro-util::make-keyword-list ,output-name ,channel-count))
	    :get-output (lambda (output)
			  ;; TODO: Realize with cond. Did not get this to work :(
			  (block nil
			    ;; generate if-clauses
			    ,@(let ((c nil))
				   (dotimes (o channel-count)
				     (push `(if (eq ,(cl-synthesizer-macro-util::make-keyword output-name o) output)
						(return ,(cl-synthesizer-macro-util::make-package-symbol output-name o))) c))
				   c)
			    (error (format nil "Unknown output ~a requested from ~a" output name))))
	    :update (lambda (&key ,@(cl-synthesizer-macro-util::make-param-list input-name channel-count))
		      ;; validate inputs
		      ,@(let ((c nil))
			     (dotimes (i channel-count)
			       (push `(if (not ,(cl-synthesizer-macro-util::make-package-symbol input-name i))
					  (error (format nil "Channel ~a must not be nil"
							 ,(cl-synthesizer-macro-util::get-socket-number i)))) c))
			     c)
		      ;; update
		      ,@(let ((c nil))
			     (dotimes (i channel-count)
			       (push `(push (input-to-wave
					     ,(cl-synthesizer-macro-util::make-package-symbol input-name i)) frames) c)
			       (push `(setf ,(cl-synthesizer-macro-util::make-package-symbol output-name i)
					    ,(cl-synthesizer-macro-util::make-package-symbol input-name i)) c))
			     c)
		      nil)
	    :shutdown (lambda ()
			(let ((wave (cl-wave:open-wave filename :direction :output)))
			  (cl-wave:set-num-channels wave ,channel-count)
			  (cl-wave:set-sample-rate wave (getf environment :sample-rate))
			  (cl-wave:set-frames wave (nreverse frames))
			  (cl-wave:close-wave wave)
			  (setf frames nil)))
	    ))))))

;; http://clhs.lisp.se/Body/s_eval_w.htm#eval-when
(n-channel-wave-file-writer "one-channel-wave-file-writer" 1)
(n-channel-wave-file-writer "two-channel-wave-file-writer" 2)
(n-channel-wave-file-writer "three-channel-wave-file-writer" 3)
(n-channel-wave-file-writer "four-channel-wave-file-writer" 4)
(n-channel-wave-file-writer "five-channel-wave-file-writer" 5)
(n-channel-wave-file-writer "six-channel-wave-file-writer" 6)
(n-channel-wave-file-writer "seven-channel-wave-file-writer" 7)
(n-channel-wave-file-writer "eight-channel-wave-file-writer" 8)

;;
;; The following code is a bit crazy and needs rework :)
;;

(defparameter *wave-file-writers*
  (make-array
   8
   :initial-contents
   (list
    #'one-channel-wave-file-writer
    #'two-channel-wave-file-writer
    #'three-channel-wave-file-writer
    #'four-channel-wave-file-writer
    #'five-channel-wave-file-writer
    #'six-channel-wave-file-writer
    #'seven-channel-wave-file-writer
    #'eight-channel-wave-file-writer)))

(defun get-n-channel-wave-file-writer (channel-count)
  "Returns a creator function for a wave-writer with given number of channels
   - channel-count: Number of channels"
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "channel-count must be greater than 0: ~a"
       :format-arguments (list channel-count)))
  (if (> channel-count (length *wave-file-writers*))
      (cl-synthesizer:signal-assembly-error
       :format-control "channel-count must be smaller than ~a: ~a"
       :format-arguments (list (length *wave-file-writers*) channel-count)))
  (elt *wave-file-writers* (- channel-count 1)))