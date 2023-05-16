(in-package :cl-synthesizer-test)


(define-test test-rack-hook-1 ()
  (let ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	(hook-1-shutdown-count 0)
	(hook-1-update-count 0)
	(hook-2-shutdown-count 0)
	(hook-2-update-count 0))
    (cl-synthesizer:add-hook
     rack
     (list
      :update (lambda() (setf hook-1-update-count (+ 1 hook-1-update-count)))
      :shutdown (lambda() (setf hook-1-shutdown-count (+ 1 hook-1-shutdown-count)))))
    (cl-synthesizer:add-hook
     rack
     (list
      :update (lambda() (setf hook-2-update-count (+ 1 hook-2-update-count)))
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
    (assert-equal hook-1-update-count 5)
    (assert-equal hook-2-update-count 5)))
