;;
;; A module with a fixed output value
;;
;; Intended use is for the analysis of the behaviour
;; of a component under a certain hard coded context.
;;


(in-package :cl-synthesizer-modules-fixed-output)

(defun fixed-output (name environment &key value)
  (declare (ignore name environment))
  (list
   :inputs (lambda () nil)
   :outputs (lambda () '(:out))
   :get-output (lambda (output)
		 (declare (ignore output))
		 value)
   :update (lambda () nil)))


