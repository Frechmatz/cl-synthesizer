(in-package :cl-synthesizer-test)

(defun test-module (name environment)
  "Module that mirrors its input"
  (declare (ignore environment name))
  (let ((out-1 0) (out-2 0))
    (list
     :inputs (lambda () (list :cv-1 :cv-2))
     :outputs (lambda () (list :out-1 :out-2))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out-1 output)
			out-1)
		       ((eq :out-2 output)
			out-2)
		       (t (error (format nil "Unknown output ~a requested from test-module" output)))))
     :update (lambda (input-args
		      ;;&key cv-1 cv-2
			  )
	       (let ((cv-1 (getf input-args :cv-1))
		     (cv-2 (getf input-args :cv-2)))
	       (if cv-1
		   (setf out-1 cv-1))
	       (if cv-2
		   (setf out-2 cv-2)))
     ))))

(defun test-module-counter (name environment)
  "Module that increments its output on each tick"
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () nil)
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from test-module" output)))))
     :update (lambda (input-args)
	       (declare (ignore input-args))
	       (setf out (+ 1 out)))
     )))

(defun test-module-multiply-by-two (name environment)
  "Module that multiplies its input by two"
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () (list :in))
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from test-module-multiply-by-two" output)))))
     :update (lambda (input-args
		      ;;&key in
			  )
	       (setf out (* 2 (getf input-args :in))))
     )))

(defun test-module-adder (name environment)
  "Module that increments its output on each tick"
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () (list :in-1 :in-2))
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from test-module-add" output)))))
     :update (lambda (input-args
		      ;;&key in-1 in-2
			  )
	       (let ((in-1 (getf input-args :in-1))
		     (in-2 (getf input-args :in-2)))
	       (setf out (+ in-1 in-2)))
     ))))

(defun test-module-multiple-1-2 (name environment)
  ""
  (declare (ignore environment name))
  (let ((out 0))
    (list
     :inputs (lambda () (list :in))
     :outputs (lambda () (list :out-1 :out-2))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out-1 output) out)
		       ((eq :out-2 output) out)
		       (t (error (format nil "Unknown output ~a requested from test-module-multiple-1-2" output)))))
     :update (lambda (input-args
		      ;;&key in
			  )
	       (setf out (getf input-args :in)))
     )))


(defmacro expect-assembly-exception(&body body)
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer::assembly-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

(defmacro expect-invalid-arguments-exception(&body body)
  `(handler-case
       (progn
         ,@body
         (assert-true nil))
     (cl-synthesizer::invalid-arguments-error (err)
       (assert-true err) ;; increase test count of lisp-unit summary
       t)
     (error (err)
       (assert-false err)
       nil)))

(defun zero-crossing-trigger ()
  (let ((cur-value 0.0))
    (lambda (v)
      (let ((is-crossing
	     (or
	      (and (< cur-value 0.0) (< 0.0 v))
	      (and (< v 0.0) (< 0.0 cur-value)))))
	(setf cur-value v)
	is-crossing))))

(defun get-frequency (&key sample-rate update-fn get-output-fn)
  (let ((seconds 2)
	(trigger (zero-crossing-trigger))
	(zero-crossing-count 0))
    (dotimes (i (* seconds sample-rate))
      (funcall update-fn)
      (if (funcall trigger (funcall get-output-fn))
	  (setf zero-crossing-count (+ 1 zero-crossing-count))))
    (float (/ zero-crossing-count (* seconds 2)))))

;;  4.0 4.0001 0.01 => t
;; -4.0 4.0001 0.01 => nil
;;  4.0 -4.0001 0.01 => nil
;;  0.0 0.001  0.01 => t
;;  0.0 0.1    0.01 => nil
(defun is-approximately (expected-number number allowed-deviation)
  (let ((diff (- expected-number number)))
    (< (abs diff) allowed-deviation)))

  

  

  
  

