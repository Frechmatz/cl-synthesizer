(in-package :cl-synthesizer-test)

(defparameter *CC-HANDLER-TEST-VENDOR*
  (list
   :get-controller-number
   (lambda (id)
     (getf '(:ENCODER-1 112 :ENCODER-2 74) id))
   :get-controller-value-offset
   (lambda (controller-value)
     (cond
       ((eq 61 controller-value) -5)
       ((eq 62 controller-value) -3)
       ((eq 63 controller-value) -1)
       ((eq 65 controller-value) 1)
       ((eq 66 controller-value) 3)
       ((eq 67 controller-value) 5)
       (t 0)))))

(defparameter *CC-HANDLER-NG-INITIAL-CV* 5.0)

(defun run-cc-ng-testcase (&key
			     (cc-setup nil)
			     (events nil)
			     (duplicate-event-count 1)
			     (call-update-count 1)
			     (cv-min 0)
			     (cv-max 100)
			     (cv-initial *CC-HANDLER-NG-INITIAL-CV*)
			     result)
  ":cc-setup -- (list (list :ENCODER-2 0.01) (list :ENCODER-3 0.1))
   :duplicate-events 1
   :call-update-count 1
   :events -- (list (list :ENCODER-1 67) (list :ENCODER-2 68))
   :result *CC-HANDLER-NG-INITIAL-CV*"
  (let ((handler (funcall
		  #'cl-synthesizer-midi:relative-cc-handler
		  *CC-HANDLER-TEST-VENDOR*
		  (mapcar
		   (lambda (setup-entry)
		     (list :controller-id (first setup-entry) :delta-percent (second setup-entry)))
		   cc-setup)
		  :cv-initial cv-initial :cv-min cv-min :cv-max cv-max)))
    (let ((midi-events nil))
      (dotimes (i duplicate-event-count)
	(dolist (evt events)
	  (let ((controller-number (funcall (getf *CC-HANDLER-TEST-VENDOR* :get-controller-number) (first evt)))
		(event-value (second evt)))
	    (push (cl-synthesizer-midi-event:make-control-change-event 1 controller-number event-value)
		  midi-events))))
      (if midi-events
	  (dotimes (i call-update-count)
	    (funcall (getf handler :update) (reverse midi-events))))
      (assert-equal result (funcall (getf handler :get-output))))))

(define-test test-cc-ng-1 ()
  (run-cc-ng-testcase :cc-setup nil :events nil :result *CC-HANDLER-NG-INITIAL-CV*))

(define-test test-cc-ng-3 ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 0))
   :result *CC-HANDLER-NG-INITIAL-CV*))

(define-test test-cc-inc-by-one-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 65) (list :ENCODER-2 65))
   :result (+ 1 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-inc-by-two-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 65) (list :ENCODER-1 65) (list :ENCODER-2 65))
   :result (+ 2 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-inc-by-three-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 66) (list :ENCODER-2 65))
   :result (+ 3 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-inc-by-five-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 67) (list :ENCODER-2 65))
   :result (+ 5 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-inc-clip-top-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 67) (list :ENCODER-1 67) (list :ENCODER-2 65))
   :call-update-count 100
   :result 100))

(define-test test-cc-dec-by-one-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 63) (list :ENCODER-2 65))
   :result (- *CC-HANDLER-NG-INITIAL-CV* 1)))

(define-test test-cc-dec-by-two-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 63) (list :ENCODER-1 63) (list :ENCODER-2 65))
   :result (- *CC-HANDLER-NG-INITIAL-CV* 2)))

(define-test test-cc-dec-by-three-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 62) (list :ENCODER-2 65))
   :result (- *CC-HANDLER-NG-INITIAL-CV* 3)))

(define-test test-cc-dec-by-five-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 61) (list :ENCODER-2 65))
   :result (- *CC-HANDLER-NG-INITIAL-CV* 5)))

(define-test test-cc-inc-clip-bottom-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01) (list :ENCODER-1 0.01) (list :ENCODER-2 65))
   :call-update-count 100
   :events (list (list :ENCODER-1 61))
   :result 0))

(define-test test-cc-update-multiple-events-1-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :events (list (list :ENCODER-1 65) (list :ENCODER-1 63) (list :ENCODER-2 65))
   :result *CC-HANDLER-NG-INITIAL-CV*))

(define-test test-cc-inc-by-ten-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01) (list :ENCODER-2 0.1))
   :events (list (list :ENCODER-1 0) (list :ENCODER-2 65))
   :result (+ 10 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-inc-by-twenty-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01) (list :ENCODER-2 0.1))
   :events (list (list :ENCODER-1 0) (list :ENCODER-2 65) (list :ENCODER-2 65))
   :result (+ 20 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-inc-by-eleven-ng ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01) (list :ENCODER-2 0.1))
   :events (list (list :ENCODER-1 65) (list :ENCODER-2 65))
   :result (+ 11 *CC-HANDLER-NG-INITIAL-CV*)))

(define-test test-cc-negative-cv-min-1 ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :cv-min -5
   :cv-max 5
   :cv-initial 2
   :events (list (list :ENCODER-1 65))
   :result 2.1))

(define-test test-cc-negative-cv-min-2 ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :cv-min -3
   :cv-max 7
   :cv-initial 2
   :events (list (list :ENCODER-1 65))
   :result 2.1))

(define-test test-cc-negative-cv-min-3 ()
  (run-cc-ng-testcase
   :cc-setup (list (list :ENCODER-1 0.01))
   :cv-min -3
   :cv-max 7
   :cv-initial 2
   :events (list (list :ENCODER-1 63))
   :call-update-count 100
   :result -3))

(define-test test-cc-invalid-cv-1 ()
	     (expect-assembly-exception 
	       (run-cc-ng-testcase
		:cc-setup (list (list :ENCODER-1 0.01))
		:cv-min 5
		:cv-max -5
		:cv-initial 2
		:events (list (list :ENCODER-1 63))
		:result -3)))

(define-test test-cc-invalid-initial-cv-1 ()
	     (expect-assembly-exception 
	       (run-cc-ng-testcase
		:cc-setup (list (list :ENCODER-1 0.01))
		:cv-min -5
		:cv-max 5
		:cv-initial 20
		:events (list (list :ENCODER-1 63))
		:result -3)))

(define-test test-cc-equal-min-max-cv-1 ()
	     (run-cc-ng-testcase
	      :cc-setup (list (list :ENCODER-1 0.01))
	      :cv-min 3
	      :cv-max 3
	      :cv-initial 3
	      :events (list (list :ENCODER-1 63))
	      :result 3.0))
