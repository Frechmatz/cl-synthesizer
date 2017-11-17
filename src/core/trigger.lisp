;;
;;
;; A component that generates trigger events
;;
;;

(in-package :cl-synthesizer-core)

(defun trigger (&key switching-voltage)
  "A factory function for a trigger component. 
   The generated function fires a one clock cycle long pulse when input-voltage >= switching-voltage,
   then stops firing until the input-voltage went below the switching-voltage.
   Parameters:
   - switching-voltage: The minimum value of the input voltage in order to fire.
   See also: cl-synthesizer-core:gate"
  (let ((is-wait nil))
    (list
     :is-firing (lambda (v)
		   (let ((may-fire (>= v switching-voltage)))
		     (cond
		       ((and is-wait may-fire)
			nil)
		       (is-wait
			;; Waiting and voltage is below switching voltage -> reset
			(setf is-wait nil)
			nil)
		       (may-fire
			(setf is-wait t)
			t)
		       (t nil))))
     :reset (lambda () (setf is-wait nil)))))
