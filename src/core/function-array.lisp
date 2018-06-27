(in-package :cl-synthesizer-core)

(defun function-array (array-functions)
  "Returns a controller function that iterates through a list of so called array-functions.
   An array function is defined as a plist with the following properties:
   - :init -- A function with no arguments that is called when the controller switches to the array-function.
   - :update -- A function with no arguments that is called in order to evaluate the array-function (to process a tick).
   The update function of the array-function must return :DONE or :CONTINUE. In the case of :CONTINUE
   the controller switches forward to the next array-function (if available) and 
   initializes and updates it.
   Returns a function with the following arguments:
   - restart -- If t the controller switches to the first array-function. Otherwise
   the controller continues with the current array-function or does nothing if there is no current array-function.
   The function returns the current array-function index or nil."
  (if (or (not array-functions) (= 0 (length array-functions)))
      (cl-synthesizer:signal-assembly-error
       :format-control "function-array: Function array must not be nil or empty"
       :format-arguments (list)))
  (let ((cur-function-index nil)
	(array-function-array
	 (make-array
	  (length array-functions)
	  :initial-contents array-functions)))
    (labels ((activate-array-function (index)
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       (setf cur-function-index index)
	       (funcall (getf (elt array-function-array cur-function-index) :init)))
	     (activate-next-array-function ()
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       (if (>= (+ 1 cur-function-index) (length array-functions))
		   (progn
		     (setf cur-function-index nil)
		     nil)
		   (progn
		     (activate-array-function (+ 1 cur-function-index))
		     t)))
	     (update-array-function (restart)
	       ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	       (if restart
		   (progn
		     (activate-array-function 0)
		     (update-array-function nil))
		   (if cur-function-index
		       (let ((function-result (funcall (getf (elt array-function-array cur-function-index) :update))))
			 (if (and (eq :CONTINUE function-result) (activate-next-array-function))
			     (update-array-function nil)))))))
      (lambda (restart)
	(update-array-function restart)
	cur-function-index))))
