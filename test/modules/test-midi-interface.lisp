(in-package :cl-synthesizer-test)

;;
;; voice tests
;;

(define-test test-voice-manager-voice-1 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal nil (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-2 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (cl-synthesizer-modules-midi-interface::push-note v 1)
	       (assert-true (cl-synthesizer-modules-midi-interface::is-note v 1))
	       (assert-nil (cl-synthesizer-modules-midi-interface::is-note v 99))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-3 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal 2 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-4 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 2))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-5 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 1))
	       (assert-equal :VOICE-NOTE-STACK-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 2))
	       (assert-equal nil (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-6 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 3))
	       (assert-equal 2 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-7 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-ALREADY-PRESENT (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 2))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))
	       (assert-equal :VOICE-NOTE-STACK-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 1))))

(define-test test-voice-manager-voice-8 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 2))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 3))
	       (assert-equal 3 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-9 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-OVERWRITE (cl-synthesizer-modules-midi-interface::push-note v 2))
	       (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 1))
	       (assert-equal :VOICE-NOTE-STACK-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 2))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-10 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (assert-equal :VOICE-NOTE-INIT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-ALREADY-PRESENT (cl-synthesizer-modules-midi-interface::push-note v 1))
	       (assert-equal :VOICE-NOTE-STACK-EMPTY (cl-synthesizer-modules-midi-interface::remove-note v 1))))

;;
;; voice-manager tests
;;

(define-test test-voice-manager-mgr-0 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (assert-equal 0 (cl-synthesizer-modules-midi-interface::find-voice-index-by-note mgr 11))))

(define-test test-voice-manager-mgr-1 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (assert-equal nil (cl-synthesizer-modules-midi-interface::get-voice-note mgr 255))))

(define-test test-voice-manager-mgr-2 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state)
		 (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0)))))

(define-test test-voice-manager-mgr-3 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (assert-equal 22 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-4 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::remove-note mgr 22)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY voice-state))
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-5 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-ALREADY-PRESENT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::remove-note mgr 22)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY voice-state))
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-6 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22)
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 22 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))))

(define-test test-voice-manager-mgr-7 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22)
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 33)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (assert-equal 33 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 22 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))))

(define-test test-voice-manager-mgr-8 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22)
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 33)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 44)
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (assert-equal 33 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 44 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))))

(define-test test-voice-manager-mgr-9 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11) ;; 0 -> 11
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22) ;; 1 -> 22
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 33) ;; 0 -> 33
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::remove-note mgr 33) ;; 0 -> 11
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY voice-state))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 44) ;; 1 -> 44
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 44 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))))

(define-test test-voice-manager-mgr-10 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 11) ;; 0 -> 11
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 22) ;; 1 -> 22
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-INIT voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 33) ;; 0 -> 33
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 44) ;; 1 -> 44
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 55) ;; 0 -> 55
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 66) ;; 1 -> 66
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (assert-equal 55 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 66 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::remove-note mgr 66)
		 (assert-equal 1 voice-number)
		 (assert-equal :VOICE-NOTE-STACK-NOT-EMPTY voice-state))
	       (assert-equal 44 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::push-note mgr 77) ;; 0 -> 77
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-OVERWRITE voice-state))
	       (assert-equal 77 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-11 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11) ;; 0 -> 11
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::remove-note mgr 11)
		 (assert-equal 0 voice-number)
		 (assert-equal :VOICE-NOTE-STACK-EMPTY voice-state))
	       (multiple-value-bind (voice-number voice-state)
		   (cl-synthesizer-modules-midi-interface::remove-note mgr 11)
		 (assert-nil voice-number)
		 (assert-nil voice-state))))
