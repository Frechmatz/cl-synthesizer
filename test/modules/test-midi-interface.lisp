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
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-3 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (cl-synthesizer-modules-midi-interface::push-note v 1)
	       (cl-synthesizer-modules-midi-interface::push-note v 2)
	       (assert-equal 2 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-4 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (cl-synthesizer-modules-midi-interface::push-note v 1)
	       (cl-synthesizer-modules-midi-interface::push-note v 2)
	       (cl-synthesizer-modules-midi-interface::remove-note v 2)
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-5 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (cl-synthesizer-modules-midi-interface::push-note v 1)
	       (cl-synthesizer-modules-midi-interface::push-note v 2)
	       (cl-synthesizer-modules-midi-interface::remove-note v 1)
	       (cl-synthesizer-modules-midi-interface::remove-note v 2)
	       (cl-synthesizer-modules-midi-interface::remove-note v 2)
	       (assert-equal nil (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-6 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (cl-synthesizer-modules-midi-interface::push-note v 1)
	       (cl-synthesizer-modules-midi-interface::push-note v 2)
	       (cl-synthesizer-modules-midi-interface::remove-note v 3)
	       (assert-equal 2 (cl-synthesizer-modules-midi-interface::get-note v))))

(define-test test-voice-manager-voice-7 ()
	     (let ((v (make-instance 'cl-synthesizer-modules-midi-interface::voice)))
	       (cl-synthesizer-modules-midi-interface::push-note v 1)
	       (cl-synthesizer-modules-midi-interface::push-note v 2)
	       (cl-synthesizer-modules-midi-interface::push-note v 2)
	       (cl-synthesizer-modules-midi-interface::remove-note v 2)
	       (assert-equal 1 (cl-synthesizer-modules-midi-interface::get-note v))))

;;
;; voice-manager tests
;;

(define-test test-voice-manager-mgr-1 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (assert-equal nil (cl-synthesizer-modules-midi-interface::get-voice-note mgr 255))))

(define-test test-voice-manager-mgr-2 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-3 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (assert-equal 22 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-4 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (cl-synthesizer-modules-midi-interface::remove-note mgr 22)
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-5 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 1)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (cl-synthesizer-modules-midi-interface::remove-note mgr 22)
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))))

(define-test test-voice-manager-mgr-6 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 22 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       ))

(define-test test-voice-manager-mgr-7 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 33)
	       (assert-equal 33 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 22 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       ))

(define-test test-voice-manager-mgr-8 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 33)
	       (cl-synthesizer-modules-midi-interface::push-note mgr 44)
	       (assert-equal 33 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 44 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       ))

(define-test test-voice-manager-mgr-9 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11) ;; 0 -> 11
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22) ;; 1 -> 22
	       (cl-synthesizer-modules-midi-interface::push-note mgr 33) ;; 0 -> 33
	       (cl-synthesizer-modules-midi-interface::remove-note mgr 33) ;; 0 -> 11
	       (cl-synthesizer-modules-midi-interface::push-note mgr 44) ;; 1 -> 44
	       (assert-equal 11 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 44 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       ))

(define-test test-voice-manager-mgr-10 ()
	     (let ((mgr (make-instance 'cl-synthesizer-modules-midi-interface::voice-manager :voice-count 2)))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 11) ;; 0 -> 11
	       (cl-synthesizer-modules-midi-interface::push-note mgr 22) ;; 1 -> 22
	       (cl-synthesizer-modules-midi-interface::push-note mgr 33) ;; 0 -> 33
	       (cl-synthesizer-modules-midi-interface::push-note mgr 44) ;; 1 -> 44
	       (cl-synthesizer-modules-midi-interface::push-note mgr 55) ;; 0 -> 55
	       (cl-synthesizer-modules-midi-interface::push-note mgr 66) ;; 1 -> 66
	       (assert-equal 55 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       (assert-equal 66 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       (cl-synthesizer-modules-midi-interface::remove-note mgr 66)
	       (assert-equal 44 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 1))
	       (cl-synthesizer-modules-midi-interface::push-note mgr 77) ;; 0 -> 77
	       (assert-equal 77 (cl-synthesizer-modules-midi-interface::get-voice-note mgr 0))
	       ))


