(in-package :cl-synthesizer-midi)

(defun cc-mapper ()
  (let ((map-fns nil))
    (labels ((get-value (controller-number control-value)
	     (let ((value nil))
	       (dolist (fn map-fns)
		 (setf value (funcall fn controller-number control-value))
		 (if value
		     (return)))
	       value))
	   (assert-not-mapped (controller-number control-value)
	     (let ((cur-value (get-value controller-number control-value)))
	       (if cur-value
		   (cl-synthesizer:signal-assembly-error
		    :format-control "cc mapping of controller-number ~a control-value ~a already set to ~a"
		    :format-arguments (list controller-number control-value cur-value))))))
      (list
       :add-mapping
       (lambda(controller-number control-value value)
	 (assert-not-mapped controller-number control-value)
	 (push
	  (lambda(ctrl-number ctrl-value)
	    (if (and (eql ctrl-number controller-number)
		     (eql ctrl-value control-value))
		value
		nil))
	  map-fns))
       :add-list-mapping
       (lambda(controller-number control-values value)
	 (dolist (control-value control-values)
	   (assert-not-mapped controller-number control-value))
	 (push
	  (lambda(ctrl-number ctrl-value)
	    (if (not (eql ctrl-number controller-number))
		nil
		(if (find ctrl-value control-values :test #'eql)
		    value
		    nil)))
	  map-fns))
       :add-range-mapping
       (lambda(controller-number from to value)
	 (dotimes (ctrl-value (- to from))
	   (assert-not-mapped controller-number (+ ctrl-value from)))
	 (push
	  (lambda(ctrl-number ctrl-value)
	    (if (not (eql ctrl-number controller-number))
		nil
		(if (and (>= from ctrl-value) (<= ctrl-value to))
		    value
		    nil)))
	  map-fns))
       :map
       (lambda (controller-number control-value)
	 (get-value controller-number control-value))))))
