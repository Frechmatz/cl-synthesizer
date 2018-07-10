(in-package :cl-synthesizer-core)

(defun function-array (array-functions)
  "Returns a function that iterates through a list of so called array-functions.
    An array function is defined as a property list with the following keys:
    <ul>
	<li>:init A function with no arguments that is called by the controller on entry of the array-function.</li>
	<li>:update A function with no arguments that is called by the controller to determine
	    how to continue. The function must return one of the following values:
	    <ul>
		<li>:DONE The controller keeps the current array-function and will continue to call its update function.</li>
		<li>:CONTINUE The controller switches to the next array-function and calls
		    its :init and consecutively its :update function.</li>
	    </ul>
	</li>
    </ul>
    Returns a function with the following arguments:
    <ul>
	<li>restart If t the controller switches to the first array-function. Otherwise
	    the controller continues with the current array-function or does nothing if there is no current array-function.
	    The initial state of the controller is, that there is no current array-function. The function
	    processing must explicitly started by setting the restart argument to t.
	</li>
    </ul>
    <p>
	Example:
	<pre><code>
        (let* ((counter nil)
               (f (cl-synthesizer-core:function-array
                   (list
                    (list :init (lambda() (setf counter 0))
                          :update (lambda()
                                    (format t \"First function: counter is ~a~%\" counter)
                                    (setf counter (+ 1 counter))
                                    (if (< counter 10) :DONE :CONTINUE)))
                    (list :init (lambda() (setf counter 0))
                          :update (lambda()
                                    (format t \"Second function: counter is ~a~%\" counter)
                                    (setf counter (+ 1 counter))
                                    (if (< counter 5) :DONE :CONTINUE)))))))
          (funcall f t)
          (dotimes (i 100)
            (funcall f nil)))
	</code></pre>
    </p>
    <p>
	The console output of the example is:
	<pre>
        First function: counter is 0
        First function: counter is 1
        First function: counter is 2
        First function: counter is 3
        First function: counter is 4
	First function: counter is 5
	First function: counter is 6
	First function: counter is 7
	First function: counter is 8
	First function: counter is 9
	Second function: counter is 0
	Second function: counter is 1
	Second function: counter is 2
	Second function: counter is 3
	Second function: counter is 4
	</pre>
    </p>"
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
	nil))))
