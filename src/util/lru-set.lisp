(in-package :cl-synthesizer-lru-set)

;;
;; Entry
;;

(defclass lru-entry ()
  ((v :initform nil) ;; the value or nil
   (tick-counter :initform nil)
   (tick :initform 0)))

(defmethod initialize-instance :after ((v lru-entry) &key tick-counter)
  (setf (slot-value v 'tick-counter) tick-counter)
  (setf (slot-value v 'tick) (funcall tick-counter)))

(defmacro lru-entry-is-value (cur-entry v)
  `(equal ,v (slot-value ,cur-entry 'v)))

(defmacro lru-entry-get-current-value (cur-entry)
  `(slot-value ,cur-entry 'v))

(defmacro lru-entry-get-tick (cur-entry)
  `(slot-value ,cur-entry 'tick))

(defmacro lru-entry-touch (cur-entry)
  `(setf (slot-value ,cur-entry 'tick) (funcall (slot-value ,cur-entry 'tick-counter))))

;; Sets a value
(defun lru-entry-set-value (cur-entry value)
  (lru-entry-touch cur-entry)
  (setf (slot-value cur-entry 'v) value)
  value)

;; Resets the entry. Returns the current value or nil
(defun lru-entry-reset (cur-entry)
  (if (slot-value cur-entry 'v)
      (progn
	(lru-entry-touch cur-entry)
	(setf (slot-value cur-entry 'v) nil)))
  nil)

;;
;; Lru-Set
;;

(defun make-tick-counter ()
  (let ((tick 0))
    (lambda()
      (setf tick (+ 1 tick))
      tick)))

(defclass lru-set ()
  ((entries :initform nil)
   (tick-counter :initform (make-tick-counter)))
  (:documentation "A fixed capacity LRU Set. Entries are accessible by index."))

(defmethod initialize-instance :after ((mgr lru-set) &key capacity)
  (if (equal 0 capacity)
      (error "lru-set: capacity must be greater zero"))
  (let ((entry-array (make-array capacity)))
    (setf (slot-value mgr 'entries) entry-array)
    (dotimes (i capacity)
      (setf (aref entry-array i) (make-instance 'lru-entry :tick-counter (slot-value mgr 'tick-counter))))))

;; Returns an index
(defun lru-set-find-index-by-value (cur-lru-set value)
  (let ((entries (slot-value cur-lru-set 'entries)))
    (dotimes (index (length entries))
      (if (lru-entry-is-value (elt entries index) value)
	  (return index)))))

;; Returns a value
(defun current-value (cur-lru-set)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((entries (slot-value cur-lru-set 'entries))
	(max-tick-active-entry nil)
	(index-active-entry nil)
	(value-active-entry nil))
    ;; get the newest active value
    (dotimes (index (length entries))
      (let ((entry (elt entries index)))
	(let ((tick (lru-entry-get-tick entry)))
	  (if (lru-entry-get-current-value entry)
	      ;; Already active entry
	      (if (or (not max-tick-active-entry) (< max-tick-active-entry tick))
		  (progn
		    (setf value-active-entry (lru-entry-get-current-value entry))
		    (setf max-tick-active-entry tick)
		    (setf index-active-entry index)))))))
    value-active-entry))

;; Returns an index
(defun lru-set-allocate-entry (cur-lru-set value)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((entries (slot-value cur-lru-set 'entries))
	(min-tick-available-entry nil)
	(min-tick-active-entry nil)
	(index-available-entry nil)
	(index-active-entry nil))
    ;;
    ;; Collect
    ;; - the eldest available entry
    ;; - the eldest active entry
    ;;
    (dotimes (index (length entries))
      (let ((entry (elt entries index)))
	(let ((tick (lru-entry-get-tick entry)))
	  (if (not (lru-entry-get-current-value entry))
	      ;; We've found an available entry
	      (if (or (not min-tick-available-entry) (< tick min-tick-available-entry))
		  (progn
		    (setf min-tick-available-entry tick)
		    (setf index-available-entry index)))
	      ;; Already active entry
	      (if (or (not min-tick-active-entry) (< tick min-tick-active-entry))
		  (progn
		    (setf min-tick-active-entry tick)
		    (setf index-active-entry index)
		    ;; Value already in set?
		    (if (lru-entry-is-value entry value)
			(progn
			  (setf index-available-entry nil)
			  (return)))))))))
    (let ((resulting-index (if index-available-entry index-available-entry index-active-entry)))
      (lru-entry-reset (elt entries resulting-index))
      resulting-index)))

(defun push-value (cur-lru-set value)
  "Determine index (oldest not used one or oldest used one), assign value to it and 'touch' it. Returns the index."
  (let ((index (lru-set-allocate-entry cur-lru-set value)))
    (lru-entry-set-value (elt (slot-value cur-lru-set 'entries) index) value)
    index))

(defun remove-value (cur-lru-set value)
  "Remove value and 'touch' corresponding index. Returns an index or nil."
  (let ((index (lru-set-find-index-by-value cur-lru-set value)))
    (if index
	(lru-entry-reset (elt (slot-value cur-lru-set 'entries) index)))
    index))

(defun get-value (cur-lru-set index)
  "Get a value by its index."
  (let ((entry (elt (slot-value cur-lru-set 'entries) index)))
    (lru-entry-get-current-value entry)))


