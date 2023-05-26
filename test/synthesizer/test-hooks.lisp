(in-package :cl-synthesizer-test)


(define-test test-rack-hook-1 ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	(hook-1-shutdown-count 0)
	(hook-1-updated-count 0)
	(hook-1-updating-count 0)
	(hook-2-shutdown-count 0)
	(hook-2-updated-count 0)
	(hook-2-updating-count 0))
    (cl-synthesizer:add-hook
     rack
     (list
      :updated (lambda() (setf hook-1-updated-count (+ 1 hook-1-updated-count)))
      :updating (lambda() (setf hook-1-updating-count (+ 1 hook-1-updating-count)))
      :shutdown (lambda() (setf hook-1-shutdown-count (+ 1 hook-1-shutdown-count)))))
    (cl-synthesizer:add-hook
     rack
     (list
      :updated (lambda() (setf hook-2-updated-count (+ 1 hook-2-updated-count)))
      :updating (lambda() (setf hook-2-updating-count (+ 1 hook-2-updating-count)))
      :shutdown (lambda() (setf hook-2-shutdown-count (+ 1 hook-2-shutdown-count)))))
    (update-module rack nil)
    (update-module rack nil)
    (update-module rack nil)
    (update-module rack nil)
    (update-module rack nil)
    (cl-synthesizer:shutdown rack)
    (cl-synthesizer:shutdown rack)
    (assert-equal hook-1-shutdown-count 1)
    (assert-equal hook-2-shutdown-count 1)
    (assert-equal hook-1-updated-count 5)
    (assert-equal hook-2-updated-count 5)
    (assert-equal hook-1-updating-count 5)
    (assert-equal hook-2-updating-count 5)))

;; Test execution order of hooks
(define-test test-rack-hook-execution-order ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	(updating-hook-called-count 0)
	(updated-hook-called-count 0)
	(shutdown-hook-called-count 0))
    (let ((counter
	    (cl-synthesizer:add-module
	     rack
	     "Counter"
	     #'cl-synthesizer-test::update-counter-module)))
      (cl-synthesizer:add-hook
       rack
       (list
	:updating
	(lambda ()
	  (assert-equal shutdown-hook-called-count 0)
	  (assert-equal updating-hook-called-count 0)
	  (assert-equal 0 (get-module-output counter :out))
	  (setf updating-hook-called-count (+ 1 updating-hook-called-count)))
	:updated
	(lambda ()
	  (assert-equal shutdown-hook-called-count 0)
	  (assert-equal updating-hook-called-count 1)
	  (assert-equal 1 (get-module-output counter :out))
	  (setf updated-hook-called-count (+ 1 updated-hook-called-count)))
	:shutdown
	(lambda ()
	  (assert-equal shutdown-hook-called-count 0)
	  (assert-equal updating-hook-called-count 1)
	  (assert-equal updated-hook-called-count 1)
	  (assert-equal 1 (get-module-output counter :out))
	  (setf shutdown-hook-called-count (+ 1 shutdown-hook-called-count))))))
    (update-module rack nil)
    (cl-synthesizer:shutdown rack)
    (assert-equal shutdown-hook-called-count 1)))

