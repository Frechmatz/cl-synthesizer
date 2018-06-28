;;
;;
;; Generic Envelope Generator
;;
;; Work in progress
;;
;;

(in-package :cl-synthesizer-modules-envelope)

(defun validate-duration-controller (controller)
  (if (or
       (not (getf controller :socket))
       (not (getf controller :input-min))
       (not (getf controller :input-max))
       (not (getf controller :rel-ms-min))
       (not (getf controller :rel-ms-max)))
      (cl-synthesizer:signal-assembly-error
       :format-control "Invalid duration-controller settings: ~a"
       :format-arguments (list controller))))

(defun validate-target-cv-controller (controller)
  (if (or
       (not (getf controller :socket))
       (not (getf controller :input-min))
       (not (getf controller :input-max))
       (not (getf controller :rel-cv-min))
       (not (getf controller :rel-cv-max)))
      (cl-synthesizer:signal-assembly-error
       :format-control "Invalid target-cv-controller settings: ~a"
       :format-arguments (list controller))))

#|
required-gate-state    target-cv     duration-ms   Action
:ignore                nil           nil           Error
:ignore                nil           t             Ok
:ignore                t             nil           Error
:ignore                t             t             Ok
:on                    *             *             Ok
:off                   *             *             Ok

duration-controller    duration-ms                
t                      nil                         Error
t                      t                           Validate controller settings 

target-cv-controller   target-cv
t                      nil                         Error
t                      t                           Validate controller settings
|#
(defun validate-segment (segment)
  "Perform some basic plausibility checks of a segment definition"
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
    (if (getf segment :duration-controller)
	(validate-duration-controller (getf segment :duration-controller)))
    (if (getf segment :control-cv-controller)
	(validate-target-cv-controller (getf segment :control-cv-controller)))))

(defmacro with-gate-check (&body body)
  `(cond
    ((and (eq :on required-gate-state) (not is-gate))
     :CONTINUE)
    ((and (eq :off required-gate-state) is-gate)
     :CONTINUE)
    (t
     ,@body)))

(defun envelope (name environment &key segments (gate-trigger-threshold-cv 4.9))
  (declare (ignore name))
  (let ((segment-def nil)
	(is-gate nil)
	(cur-cv 0)
	(ticks-per-ms (floor (/ (getf environment :sample-rate) 1000)))
	(controller-inputs nil)
	(controller-values nil)
	(module-inputs '(:gate)))
    (dolist (segment segments)
      (validate-segment segment)
      (let ((update-fn nil)
	    (duration-controller (getf segment :duration-controller))
	    (target-cv-controller (getf segment :target-cv-controller))
	    (required-gate-state (getf segment :required-gate-state))
	    (target-cv (getf segment :target-cv))
	    (duration-ms (getf segment :duration-ms)))
	(if duration-controller
	    (let ((socket (getf duration-controller :socket)))
	      (if (find socket module-inputs)
		  (cl-synthesizer:signal-assembly-error
		   :format-control "Controller socket ~a is a reserved socket identifier"
		   :format-arguments (list socket)))
	      (if (find socket controller-inputs)
		  (cl-synthesizer:signal-assembly-error
		   :format-control "Controller socket not unique: ~a"
		   :format-arguments (list socket)))
	      ;; add to property list
	      (push nil controller-values)
	      (push socket controller-values)
	      (push socket controller-inputs)))
	(if target-cv-controller
	    (let ((socket (getf target-cv-controller :socket)))
	      (if (find socket module-inputs)
		  (cl-synthesizer:signal-assembly-error
		   :format-control "Controller socket ~a is a reserved socket identifier"
		   :format-arguments (list socket)))
	      (if (find socket controller-inputs)
		  (cl-synthesizer:signal-assembly-error
		   :format-control "Controller socket not unique: ~a"
		   :format-arguments (list socket)))
	      ;; add to property list
	      (push nil controller-values)
	      (push socket controller-values)
	      (push socket controller-inputs)))
	(push 
	 (list
	  :init (lambda ()
		  (cond
		    ((and duration-ms (= 0 duration-ms))
		     (setf update-fn (lambda () :CONTINUE))) 
		    ((and target-cv duration-ms)
		     (let* ((elapsed-ticks 0)
			    (total-ticks (* ticks-per-ms duration-ms))
			    (converter (cl-synthesizer-core:linear-converter
					:input-min 0
					:input-max total-ticks
					:output-min cur-cv
					:output-max target-cv)))
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
       :update (lambda (&key (gate 0))
		 (let ((previous-gate is-gate) (restart nil))
		   (setf is-gate (if (>= gate gate-trigger-threshold-cv) t nil))
		   (setf restart (and is-gate (not previous-gate)))
		   (if restart
		       (setf cur-cv 0))
		   (funcall controller restart)))))))

