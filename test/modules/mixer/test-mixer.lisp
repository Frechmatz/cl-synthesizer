(in-package :cl-synthesizer-test)

(define-test test-mixer-1 ()
	     "Test one channel (no attenuation)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 1
			   :channel-cv-max 5.0
			   :channel-cv-gain 5.0
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0)
	       (assert-equal 1.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-2 ()
	     "Test two channels (no attenuation)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 5.0
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0)
	       (assert-equal 4.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-3 ()
	     "Test two channel (internal main attenuation 50%)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 5.0
			   :main-cv-max 5.0
			   :main-cv-gain 2.5) ;; divide by 2
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0)
	       (assert-equal 2.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-4 ()
	     "Test one channel (internal channel attenuation 50%)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 1
			   :channel-cv-max 5.0
			   :channel-cv-gain 2.5
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 10.0)
	       (assert-equal 5.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-5 ()
	     "Test two channels (internal channel attenuation 50%)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 2.5
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 10.0 :channel-2 100.0)
	       (assert-equal 55.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-6 ()
	     "Test two channels (external main attenuation 50%)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 5.0
			   :main-cv-max 5.0
			   :main-cv-gain 0.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0 :cv-main 2.5)
	       (assert-equal 2.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-7 ()
	     "Test one channel (external channel attenuation 50%)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 1
			   :channel-cv-max 5.0
			   :channel-cv-gain 0.0
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 10.0 :cv-1 2.5)
	       (assert-equal 5.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-8 ()
	     "Test two channels (external channel attenuation 50%)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 0.0
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 10.0 :channel-2 100.0 :cv-1 2.5 :cv-2 2.5)
	       (assert-equal 55.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-9 ()
	     "Test two channels (external and internal main attenuation)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 5.0
			   :main-cv-max 5.0
			   :main-cv-gain 2.5)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0 :cv-main 2.5)
	       (assert-equal 4.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-10 ()
	     "Test two channels (external and internal main attenuation)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 5.0
			   :main-cv-max 5.0
			   :main-cv-gain 2.5)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0 :cv-main -2.5)
	       (assert-equal 0.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-11 ()
	     "Test two channels (external and internal channel attenuation)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 2.5
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0 :cv-1 2.5 :cv-2 2.5)
	       (assert-equal 4.0 (funcall (getf mixer :get-output) :output))))

(define-test test-mixer-12 ()
	     "Test two channels (external and internal channel attenuation)"
	     (let ((mixer (cl-synthesizer-modules-mixer:make-module
			   "Mixer"
			   (cl-synthesizer:make-environment)
			   :channel-count 2
			   :channel-cv-max 5.0
			   :channel-cv-gain 2.5
			   :main-cv-max 5.0
			   :main-cv-gain 5.0)
		     ))
	       (funcall (getf mixer :update) :channel-1 1.0 :channel-2 3.0 :cv-1 -2.5 :cv-2 2.5)
	       (assert-equal 3.0 (funcall (getf mixer :get-output) :output))))

