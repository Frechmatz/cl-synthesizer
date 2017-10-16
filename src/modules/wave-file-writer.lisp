(in-package :cl-synthesizer-modules-wave-file-writer)

(defun wave-writer-float-to-int16 (value)
  (if (> value 1.0)
      (break))
  (if (< value -1.0)
      (break))
  (let ((i (round (* 32000 value))))
    i))

(eval-when (:compile-toplevel) 
  (defun get-socket-number (i)
    (+ i 1)))

(eval-when (:compile-toplevel) 
  (defun make-wave-writer-symbol-impl (name num package)
    (if num
	(intern (format nil "~a-~a" (string-upcase name) (get-socket-number num)) package)
	(intern (string-upcase name) package))))

(eval-when (:compile-toplevel) 
  (defun make-wave-writer-symbol (name num)
    (make-wave-writer-symbol-impl name num "CL-SYNTHESIZER-MODULES-WAVE-FILE-WRITER")))

(eval-when (:compile-toplevel) 
  (defun make-keyword (name num)
    (make-wave-writer-symbol-impl name num "KEYWORD")))

(eval-when (:compile-toplevel) 
  (defun make-let-list (name count)
    (let ((l nil))
      (dotimes (i count)
	(push (list (make-wave-writer-symbol name i) nil) l))
      l)))

(eval-when (:compile-toplevel) 
  (defun make-param-list (name count)
    (let ((l nil))
      (dotimes (i count)
	(push (make-wave-writer-symbol name i) l))
      (nreverse l))))

(eval-when (:compile-toplevel) 
  (defun make-keyword-list (name count)
    (let ((l nil))
      (dotimes (i count)
	(push (make-keyword name i) l))
      (nreverse l))))

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
    `(defun ,(make-wave-writer-symbol name nil) (environment &key filename &allow-other-keys)
       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
       (let ((frames nil)
	     (inputs (make-keyword-list ,input-name ,channel-count))
	     (outputs (make-keyword-list ,output-name ,channel-count))
	     ,@(make-let-list output-name channel-count))
	 (list
	  :inputs (lambda () inputs)
	  :outputs (lambda () outputs)
	  :get-output (lambda (output)
			;; TODO: Realize with cond. Did not get this to work :(
			(block nil
			  ;; generate if-clauses
			  ,@(let ((c nil))
				 (dotimes (o channel-count)
				   (push `(if (eq ,(make-keyword output-name o) output)
					      (return ,(make-wave-writer-symbol output-name o))) c))
				 c)
			  (error (format nil "Unknown output ~a requested from channel-wave-file-writer" output))))
	  :update (lambda (&key ,@(make-param-list input-name channel-count))
		    ;; validate inputs
		    ,@(let ((c nil))
			   (dotimes (i channel-count)
			     (push `(if (not ,(make-wave-writer-symbol input-name i))
					(error (format nil "Channel ~a must not be nil" ,(get-socket-number i)))) c))
			   c)
		    ;; update
		    ,@(let ((c nil))
			   (dotimes (i channel-count)
			     (push `(push (wave-writer-float-to-int16 ,(make-wave-writer-symbol input-name i)) frames) c)
			     (push `(setf ,(make-wave-writer-symbol output-name i) ,(make-wave-writer-symbol input-name i)) c))
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

#|
(defun test ()
  (let ((w (n-channel-wave-file-writer "channel-2" 2)))
    (let ((instance (funcall w nil)))
      (format t "~%Inputs: ~a~%" (funcall (getf instance :inputs)) )
      (format t "~%Outputs: ~a~%" (funcall (getf instance :outputs)) )
      (format t "~%Output 1: ~a~%" (funcall (getf instance :get-output) :out-1) )
      (funcall (getf instance :update) :channel-1 0.5 :channel-2 0.7)
      (format t "~%Updated Output 1 is ~a~%" (funcall (getf instance :get-output) :out-1))
      (format t "~%Updated Output 2 is ~a~%" (funcall (getf instance :get-output) :out-2))
      )))

(test)
|#

(eval-when (:compile-toplevel) 
  (n-channel-wave-file-writer "one-channel-wave-file-writer" 1))

(eval-when (:compile-toplevel) 
  (n-channel-wave-file-writer "two-channel-wave-file-writer" 2))

