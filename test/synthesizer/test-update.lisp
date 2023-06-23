(in-package :cl-synthesizer-test)


;;
;; Test rack updates
;; - All modules are updated during a tick
;; - Modules are updated in correct order during a tick
;; - Modules are not updated multiple times during a tick
;;

(defun make-update-logger ()
  (let ((log nil))
    (labels ((get-position (module-name)
	       ;; The reverse is brutal :(
	       (position module-name (reverse log) :test #'string=)))
    (list
     :update
     (lambda (module-name)
       (push module-name log))
     :is-updated-before
     (lambda (module-name-before module-name-after)
       (let ((position-before (get-position module-name-before))
	     (position-after (get-position module-name-after)))
	 (< position-before position-after)))
     :is-updated
     (lambda (module-name)
       (get-position module-name))
     :length
     (lambda ()
       (length log))))))

(define-test test-rack-update-1 ()
  "Rack consisting of multiple modules and no patches"
  (let* ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	 (logger (make-update-logger))
	 (update-handler (getf logger :update)))
    (cl-synthesizer:add-module
     rack
     "Module-1"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Module-2"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Module-3"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:update rack)
    (assert-equal 3 (funcall (getf logger :length)))
    ;; Update order is not defined. Verify that all modules have been updated.
    (assert-true (funcall (getf logger :is-updated) "Module-1"))
    (assert-true (funcall (getf logger :is-updated) "Module-2"))
    (assert-true (funcall (getf logger :is-updated) "Module-3"))))

(define-test test-rack-update-2 ()
  "Patches: Module-1 => Module-2 => Module-3"
  (let* ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	 (logger (make-update-logger))
	 (update-handler (getf logger :update)))
    (cl-synthesizer:add-module
     rack
     "Module-1"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Module-2"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Module-3"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-patch
     rack
     "Module-1" :output-1
     "Module-2" :input-1)
    (cl-synthesizer:add-patch
     rack
     "Module-2" :output-1
     "Module-3" :input-1)
    (cl-synthesizer:add-module
     rack
     "Module-All-Alone"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:update rack)
    (assert-equal 4 (funcall (getf logger :length)))
    (assert-true (funcall (getf logger :is-updated-before) "Module-1" "Module-2"))
    (assert-true (funcall (getf logger :is-updated-before) "Module-2" "Module-3"))
    (assert-true (funcall (getf logger :is-updated) "Module-All-Alone"))))


(define-test test-rack-update-3 ()
  "Patches: Island-1-Module-1 => Island-1-Module-2 and Island-2-Module-1 => Island-2-Module-2"
  (let* ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	 (logger (make-update-logger))
	 (update-handler (getf logger :update)))
    (cl-synthesizer:add-module
     rack
     "Island-1-Module-1"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Island-1-Module-2"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-patch
     rack
     "Island-1-Module-1" :output-1
     "Island-1-Module-2" :input-1)
    (cl-synthesizer:add-module
     rack
     "Island-2-Module-1"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Island-2-Module-2"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-patch
     rack
     "Island-2-Module-1" :output-1
     "Island-2-Module-2" :input-1)
    (cl-synthesizer:add-module
     rack
     "Module-All-Alone"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:update rack)
    (assert-equal 5 (funcall (getf logger :length)))
    (assert-true (funcall (getf logger :is-updated-before) "Island-1-Module-1" "Island-1-Module-2"))
    (assert-true (funcall (getf logger :is-updated-before) "Island-2-Module-1" "Island-2-Module-2"))
    (assert-true (funcall (getf logger :is-updated) "Module-All-Alone"))))

(define-test test-rack-update-4 ()
  "Patches: Module-1 => Module-2 => Module-1"
  (let* ((rack (cl-synthesizer:make-rack :environment (cl-synthesizer:make-environment)))
	 (logger (make-update-logger))
	 (update-handler (getf logger :update)))
    (cl-synthesizer:add-module
     rack
     "Module-1"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-module
     rack
     "Module-2"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:add-patch
     rack
     "Module-1" :output-1
     "Module-2" :input-1)
    (cl-synthesizer:add-patch
     rack
     "Module-2" :output-1
     "Module-1" :input-1)
    (cl-synthesizer:add-module
     rack
     "Module-All-Alone"
     #'cl-synthesizer-test::update-notification-module :update-handler update-handler)
    (cl-synthesizer:update rack)
    (assert-equal 3 (funcall (getf logger :length)))
    (assert-true (funcall (getf logger :is-updated-before) "Module-1" "Module-2"))
    (assert-true (funcall (getf logger :is-updated) "Module-All-Alone"))))

