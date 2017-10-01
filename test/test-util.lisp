(in-package :cl-synthesizer-test)

(defun test-module (environment)
  "Module that mirrors its input"
  (declare (ignore environment))
  (let ((out-1 0) (out-2 0))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () (list :cv-1 :cv-2))
     :outputs (lambda () (list :out-1 :out-2))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out-1 output)
			out-1)
		       ((eq :out-2 output)
			out-2)
		       (t (error (format nil "Unknown output ~a requested from test-module" output)))))
     :update (lambda (&key (cv-1 0) (cv-2 0))
	       (if cv-1
		   (setf out-1 cv-1))
	       (if cv-2
		   (setf out-2 cv-2)))
     )))

(defun test-module-counter (environment)
  "Module that increments its output on each tick"
  (declare (ignore environment))
  (let ((out 0))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () nil)
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from test-module" output)))))
     :update (lambda ()
	       (setf out (+ 1 out)))
     )))

(defun test-module-multiply-by-two (environment)
  "Module that increments its output on each tick"
  (declare (ignore environment))
  (let ((out 0))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () (list :in))
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from test-module-multiply-by-two" output)))))
     :update (lambda (&key (in 0))
	       (setf out (* 2 in)))
     )))

(defun test-module-adder (environment)
  "Module that increments its output on each tick"
  (declare (ignore environment))
  (let ((out 0))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () (list :in-1 :in-2))
     :outputs (lambda () (list :out))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out output)
			out)
		       (t (error (format nil "Unknown output ~a requested from test-module-add" output)))))
     :update (lambda (&key (in-1 0) (in-2 0))
	       (setf out (+ in-1 in-2)))
     )))

(defun test-module-multiple-1-2 (environment)
  ""
  (declare (ignore environment))
  (let ((out 0))
    (list
     :shutdown (lambda () nil)
     :inputs (lambda () (list :in))
     :outputs (lambda () (list :out-1 :out-2))
     :get-output (lambda (output)
		     (cond 
		       ((eq :out-1 output) out)
		       ((eq :out-2 output) out)
		       (t (error (format nil "Unknown output ~a requested from test-module-multiple-1-2" output)))))
     :update (lambda (&key (in 0))
	       (setf out in))
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
