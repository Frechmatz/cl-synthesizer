(in-package :cl-synthesizer-property-list-iterator)

;;
;; Property-List iterators
;; Todos
;; - Support of declare expressions analog to dolist. Does not work right now
;;   due to required wrapping of body into a progn form
;; - after that removal of do-property-list-keys
;;

(defmacro do-property-list (plist property-key property-value &body body)
  (let ((read-key (gensym)) (cur-key (gensym)) (item (gensym)))
    `(let ((,read-key t) (,cur-key nil))
       (dolist (,item ,plist)
	 (if ,read-key
	     (progn
	       (setf ,read-key nil)
	       (setf ,cur-key ,item))
	     (progn
	       (setf ,read-key t)
	       (let ((,property-key ,cur-key) (,property-value ,item))
		 (progn ,@body))))))))


(defmacro do-property-list-keys (plist property-key &body body)
  (let ((read-key (gensym)) (cur-key (gensym)) (item (gensym)))
    `(let ((,read-key t) (,cur-key nil))
       (dolist (,item ,plist)
	 (if ,read-key
	     (progn
	       (setf ,read-key nil)
	       (setf ,cur-key ,item))
	     (progn
	       (setf ,read-key t)
	       (let ((,property-key ,cur-key))
		 (progn ,@body))))))))

