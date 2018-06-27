(in-package :cl-synthesizer-test)

#|
testcase:
(:functions 
 ((5.0 5.0) (3.0) ())
 :update-calls 
 (:restart nil nil nil nil)
 :expected-cvs 
 (5.0 5.0 3.0))
|#
(defun run-test-case-function-array (test-case)
  (declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((functions nil) (controller nil) (cur-cv nil) (recorded-cvs nil))
    (dolist (function-def (getf test-case :functions))
      (let ((cur-function-def function-def) (work-data nil))
	(push
	 (list :init
	       (lambda () (setf work-data (copy-list cur-function-def)))
	       :update (lambda()
			 (let ((cur (first work-data)))
			   (setf work-data (rest work-data))
			   (if cur
			       (progn
				 (setf cur-cv cur)
				 :DONE)
			       :CONTINUE))))
	   functions)))
    (setf controller (cl-synthesizer-core:function-array (reverse functions)))
    (dolist (update-param (getf test-case :update-calls))
      (let ((index (funcall controller (if (eq :restart update-param) t nil))))
	;;(if index
	    (push cur-cv recorded-cvs)
	;;    (format t "~%Update call has returned index nil: ~a~%" update-param)
	    ))
    (assert-equal (getf test-case :expected-cvs) (reverse recorded-cvs))))

(define-test function-array-test-1 ()
  (run-test-case-function-array
   '(:functions ((5.0) (3.0))
     :update-calls (:restart nil)
     :expected-cvs (5.0 3.0))))

(define-test function-array-test-1-2 ()
  (run-test-case-function-array
   '(:functions ((5.0) (3.0))
     :update-calls (:restart nil nil)
     :expected-cvs (5.0 3.0 3.0))))

(define-test function-array-test-2 ()
  (run-test-case-function-array
   '(:functions ((5.0) (3.0) ())
     :update-calls (:restart nil nil)
     :expected-cvs (5.0 3.0 3.0))))

(define-test function-array-test-3 ()
  (run-test-case-function-array
   '(:functions ((5.0) (3.0) ())
     :update-calls (:restart nil :restart)
     :expected-cvs (5.0 3.0 5.0))))

(define-test function-array-test-4 ()
  (run-test-case-function-array
   '(:functions ((5.0) (3.0) () (7.0))
     :update-calls (:restart nil :restart nil nil)
     :expected-cvs (5.0 3.0 5.0 3.0 7.0))))

(define-test function-array-test-5 ()
	     (expect-assembly-exception
	       (run-test-case-function-array
		'(:functions ()
		  :update-calls (:restart nil :restart nil nil)
		  :expected-cvs (5.0 3.0 5.0 3.0 7.0)))))










