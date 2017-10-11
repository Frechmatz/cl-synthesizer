(in-package :cl-synthesizer-modules)

;; todo: n-channel writer (generate with macro)


(defun wave-writer-float-to-int16 (value)
  (if (> value 1.0)
      (break))
  (if (< value -1.0)
      (break))
  (let ((i (round (* 32000 value))))
    i))

(defun make-wave-writer-symbol (name num package)
  (if (not package)
      (setf package "CL-SYNTHESIZER-MODULES"))
  (if num
      (intern (format nil "~a-~a" (string-upcase name) (+ 1 num)) package)
      (intern (string-upcase name) package)))

(defun make-let-list (name count)
  (let ((l nil))
    (dotimes (i count)
      (push (list (make-wave-writer-symbol name i nil) nil) l))
  l))

(defun make-param-list (name count)
  (let ((l nil))
    (dotimes (i count)
      (push (make-wave-writer-symbol name i nil) l))
  (nreverse l)))

(defun make-keyword-list (name count)
  (let ((l nil))
    (dotimes (i count)
      (push (make-wave-writer-symbol name i "KEYWORD") l))
    (nreverse l)))

(defmacro n-channel-wave-file-writer (name channel-count)
  `(defun ,(make-wave-writer-symbol name nil nil) (environment &key (filename "sound.wav"))
     (declare (optimize (debug 3) (speed 0) (space 0)))
     (declare (ignore environment))
     (let ((frames nil)
	   ,@(make-let-list "out" channel-count)
	   (inputs (make-keyword-list "channel" ,channel-count))
	   (outputs (make-keyword-list "out" ,channel-count)))
       (list
	:inputs (lambda () inputs)
	:outputs (lambda () outputs)
	:get-output (lambda (output)
		      ;; TODO: Realize with cond (did not get this to work with generated conditions)
		      ;; declare block to enable generated if clauses to leave function via return
		      (block nil
			;; generate if-clauses
			,@(let ((c nil))
			       (dotimes (o channel-count)
				 (push `(if (eq ,(make-wave-writer-symbol "out" o "KEYWORD") output)
					    (return ,(make-wave-writer-symbol "out" o nil))) c))
			       c)
			(error (format nil "Unknown output ~a requested from channel-wave-file-writer" output))))
	:update (lambda (&key ,@(make-param-list "channel" channel-count))
		  ;; validate inputs
		  ,@(let ((c nil))
			 (dotimes (i channel-count)
			   (push `(if (not ,(make-wave-writer-symbol "channel" i nil))
				      (error (format nil "Channel ~a must not be nil" ,(+ i 1)))) c))
			 c)
		  ;; update
		  ,@(let ((c nil))
			 (dotimes (i channel-count)
			   (push `(push (wave-writer-float-to-int16 ,(make-wave-writer-symbol "channel" i nil)) frames) c)
			   (push `(setf ,(make-wave-writer-symbol "out" i nil) ,(make-wave-writer-symbol "channel" i nil)) c))
			 c)
		  nil)
	:shutdown (lambda ()
		    (let ((wave (cl-wave:open-wave filename :direction :output)))
		      (cl-wave:set-num-channels wave 2)
		      (cl-wave:set-frames wave (nreverse frames))
		      (cl-wave:close-wave wave)
		      (setf frames nil)))
	))))

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




(n-channel-wave-file-writer "two-channel-wave-file-writer" 2)
#|
(defun two-channel-wave-file-writer (environment &key (filename "sound.wav"))
  "Writes inputs into a Wave file and mirrors inputs to outputs via out-<n>"
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (declare (ignore environment))
  ;; (break)
  (let ((frames nil) (output-channel-1 nil) (output-channel-2 nil))
    (list
     :inputs (lambda () (list :channel-1 :channel-2)) 
     :outputs (lambda () (list :out-1 :out-2))
     :get-output (lambda (output)
		   (cond 
		     ((eq :out-1 output)
		      output-channel-1)
		     ((eq :out-2 output)
		      output-channel-2)
		     (t (error (format nil "Unknown output ~a requested from two-channel-wave-file-writer" output)))))
     :update (lambda (&key channel-1 channel-2)
	       (if (not channel-1)
		   (error "Channel-1 must not be nil"))
	       (if (not channel-2)
		   (error "Channel-2 must not be nil"))
	       (push (wave-writer-float-to-int16 channel-1) frames)
	       (push (wave-writer-float-to-int16 channel-2) frames)
	       (setf output-channel-1 channel-1)
	       (setf output-channel-2 channel-2))
     :shutdown (lambda ()
		 (let ((wave (cl-wave:open-wave filename :direction :output)))
		   (cl-wave:set-num-channels wave 2)
		   (cl-wave:set-frames wave (nreverse frames))
		   (cl-wave:close-wave wave)
		   (setf frames nil))))))
|#

(n-channel-wave-file-writer "one-channel-wave-file-writer" 1)

#|
(defun one-channel-wave-file-writer (environment &key (filename "sound.wav"))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((frames nil) (output-channel-1 nil))
      (list
       :inputs (lambda () (list :channel-1)) 
       :outputs (lambda () (list :out-1))
       :get-output (lambda (output)
		     (cond 
		       ((eq :out-1 output)
			output-channel-1)
		       (t (error (format nil "Unknown output ~a requested from one-channel-wave-file-writer" output)))))
       :update (lambda (&key channel-1)
		 (if (not channel-1)
		     (error "Channel-1 must not be nil"))
		 (push (wave-writer-float-to-int16 channel-1) frames)
		 (setf output-channel-1 channel-1))
       :shutdown (lambda ()
		   (let ((wave (cl-wave:open-wave filename :direction :output)))
		     (cl-wave:set-num-channels wave 1)
		     (cl-wave:set-sample-rate wave (getf environment :sample-rate))
		     (cl-wave:set-frames wave (nreverse frames))
		     (cl-wave:close-wave wave)
		     (setf frames nil))))))
|#
