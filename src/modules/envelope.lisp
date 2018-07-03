;;
;;
;; Envelope Generator
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-envelope)

(defun validate-controller (controller module-inputs controller-inputs)
  (if (or
       (not (getf controller :socket))
       (not (getf controller :input-min))
       (not (getf controller :input-max))
       (not (getf controller :output-min))
       (not (getf controller :output-max)))
      (cl-synthesizer:signal-assembly-error
       :format-control "Invalid controller settings: ~a"
       :format-arguments (list controller)))
  (let ((socket (getf controller :socket)))
    (if (find socket module-inputs)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller input socket ~a is a reserved socket identifier"
	 :format-arguments (list socket)))
    (if (find socket controller-inputs)
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller input socket has already been declared: ~a"
	 :format-arguments (list socket)))
    (if (= (float (getf controller :input-min)) (float (getf controller :input-max)))
	(cl-synthesizer:signal-assembly-error
	 :format-control "Controller :input-min ~a must not be equal to :input-max ~a"
	 :format-arguments (list (getf controller :input-min) (getf controller :input-max))))))


#|
required-gate-state    target-cv     duration-ms   Action
------------------------------------------------------------------------------
:ignore                nil           nil           Error
:ignore                nil           t             Ok
:ignore                t             nil           Error
:ignore                t             t             Ok
:on                    *             *             Ok
:off                   *             *             Ok

duration-controller    duration-ms                
------------------------------------------------------------------------------
t                      nil                         Error
t                      t                           Validate controller settings 

target-cv-controller   target-cv
------------------------------------------------------------------------------
t                      nil                         Error
t                      t                           Validate controller settings
|#
(defun validate-segment (segment module-inputs controller-inputs)
  "Perform some basic plausibility checks of a segment definition"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((required-gate-state (getf segment :required-gate-state))
	(target-cv (getf segment :target-cv))
	(duration-ms (getf segment :duration-ms)))
    (cond
      ((and (eq :ignore required-gate-state) (not target-cv) (not duration-ms))
       (cl-synthesizer:signal-assembly-error
	:format-control "Invalid segment: ~a"
	:format-arguments (list segment)))
      ((and (eq :ignore required-gate-state) target-cv (not duration-ms))
       (cl-synthesizer:signal-assembly-error
	:format-control "Invalid segment: ~a"
	:format-arguments (list segment)))
      ((and (getf segment :duration-controller) (not duration-ms))
       (cl-synthesizer:signal-assembly-error
	:format-control "If a duration-controller is set then duration-ms must not be nil"
	:format-arguments (list segment)))
      ((and (getf segment :target-cv-controller) (not target-cv))
       (cl-synthesizer:signal-assembly-error
	:format-control "If a target-cv-controller is set then target-cv must not be nil"
	:format-arguments (list segment))))
    (let ((duration-socket nil) (cv-socket nil))
      (if (getf segment :duration-controller)
	  (progn 
	    (setf duration-socket (getf (getf segment :duration-controller) :socket))
	    (validate-controller (getf segment :duration-controller) module-inputs controller-inputs)))
      (if (getf segment :target-cv-controller)
	  (progn 
	    (setf cv-socket (getf (getf segment :target-cv-controller) :socket))
	    (validate-controller (getf segment :target-cv-controller) module-inputs controller-inputs)))
      (if (and duration-socket cv-socket (eq duration-socket cv-socket))
	  (cl-synthesizer:signal-assembly-error
	   :format-control "Controller input socket has already been declared: ~a"
	   :format-arguments (list duration-socket cv-socket))))))

(defmacro with-gate-check (&body body)
  `(cond
    ((and (eq :on required-gate-state) (not is-gate))
     :CONTINUE)
    ((and (eq :off required-gate-state) is-gate)
     :CONTINUE)
    (t
     ,@body)))

;; consider, to make a macro out of this function
(defun push-plist (alist key value)
  (push value alist)
  (push key alist)
  alist)

(defun envelope (name environment &key segments (gate-trigger-threshold-cv 4.9))
  (declare (ignore name))
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((segment-def nil)
	(is-gate nil)
	(cur-cv 0)
	(ticks-per-ms (floor (/ (getf environment :sample-rate) 1000)))
	(controller-inputs nil)
	(controller-handlers nil)
	(module-inputs '(:gate)))
    (dolist (segment segments)
      (validate-segment segment module-inputs controller-inputs)
      (let ((update-fn nil)
	    (duration-ms-offset 0.0)
	    (target-cv-offset 0.0)
	    (required-gate-state (getf segment :required-gate-state))
	    (target-cv (getf segment :target-cv))
	    (duration-ms (getf segment :duration-ms)))
	(if (getf segment :duration-controller)
	    (let* ((controller (getf segment :duration-controller))
		   (socket (getf controller :socket))
		   (transfer-fn (cl-synthesizer-core:linear-converter
				 :input-min (getf controller :input-min)
				 :input-max (getf controller :input-max)
				 :output-min (getf controller :output-min)
				 :output-max (getf controller :output-max))))
	      (setf controller-handlers
		    (push-plist
		     controller-handlers
		     socket
		     (lambda (value)
		       (declare (optimize (debug 3) (speed 0) (space 0)))
		       (setf duration-ms-offset (funcall (getf transfer-fn :get-y) value))
		       (if (> 0 duration-ms-offset)
			   (setf duration-ms-offset 0)))))
	      (push socket controller-inputs)))
	(if (getf segment :target-cv-controller)
	    (let* ((controller (getf segment :target-cv-controller))
		   (socket (getf controller :socket))
		   (transfer-fn (cl-synthesizer-core:linear-converter
				 :input-min (getf controller :input-min)
				 :input-max (getf controller :input-max)
				 :output-min (getf controller :output-min)
				 :output-max (getf controller :output-max))))
	      (setf controller-handlers
		    (push-plist
		     controller-handlers
		     socket
		     (lambda (value)
		       (declare (optimize (debug 3) (speed 0) (space 0)))
		       (setf target-cv-offset (funcall (getf transfer-fn :get-y) value)))))
	      (push socket controller-inputs)))
	(push 
	 (list
	  :init (lambda ()
		  (cond
		    ((and duration-ms (= 0 duration-ms))
		     (setf update-fn (lambda () :CONTINUE))) 
		    ((and target-cv duration-ms)
		     (let* ((elapsed-ticks 0)
			    (total-ticks (* ticks-per-ms (+ duration-ms duration-ms-offset)))
			    (converter (cl-synthesizer-core:linear-converter
					:input-min 0
					:input-max total-ticks
					:output-min cur-cv
					:output-max (+ target-cv target-cv-offset))))
		       (setf update-fn
			     (lambda ()
			       (setf elapsed-ticks (+ 1 elapsed-ticks))
			       (if (> elapsed-ticks total-ticks)
				  :CONTINUE
				 (with-gate-check
				   (setf cur-cv (funcall (getf converter :get-y) elapsed-ticks))
				   :DONE))))))
		    (target-cv
		     (setf update-fn
			   (lambda ()
			     (with-gate-check
			       (setf cur-cv target-cv)
			       :DONE))))
		    (t
		     (setf update-fn
			   (lambda ()
			     (with-gate-check 
			       :DONE))))))
	  :update (lambda() (funcall update-fn)))
	 segment-def)))
    (let ((controller (cl-synthesizer-core:function-array (reverse segment-def))))
      (list
       :inputs (lambda () (concatenate 'list module-inputs controller-inputs))
       :outputs (lambda () '(:cv))
       :get-output (lambda (output)
		     (declare (ignore output))
		     cur-cv)
       :update (lambda (&rest args)
		 (declare (optimize (debug 3) (speed 0) (space 0)))
		 ;; cl-synthesizer::rack always calls the module update-function with a property list
		 (dolist (socket controller-inputs)
		   (let ((value (getf args socket)))
		     (if value (funcall (getf controller-handlers socket) value))))
		 (let ((gate (if (getf args :gate) (getf args :gate) 0)))
		   (let ((previous-gate is-gate) (restart nil))
		     (setf is-gate (if (>= gate gate-trigger-threshold-cv) t nil))
		     (setf restart (and is-gate (not previous-gate)))
		     (if restart
			 (setf cur-cv 0))
		     (funcall controller restart))))))))

