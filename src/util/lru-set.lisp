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

(defmacro lru-entry-set-value (cur-entry value)
  `(setf (slot-value ,cur-entry 'v) ,value))

;;
;; Lru-Set
;;

(defun make-tick-counter ()
  "Entries are holding a 'tick' that indicates which entry was last used. Compared
   to the effort of managing some kind of linked list or a heap this is not too bad. However,
   this doesn't work for large sets (for which this Set implementation is not designed).
   TODO: Take care of overflows and in such case adjust the ticks of the entries."
  (let ((tick 0))
    (lambda()
      (setf tick (+ 1 tick))
      tick)))

(defclass lru-set ()
  ((entries :initform nil)
   (tick-counter :initform (make-tick-counter))
   (entry-count :initform 0))
  (:documentation
   "A fixed capacity LRU Set.
    - Focus is on zero consing. The implementation is quite CPU heavy.
    - Designed for small capacities (maybe up to 20 entries).
    - Entries are identified by their index.
    - Round robin assignment of values to indices (background of this behaviour is 
      the release phase of envelope generators).
    - Uses 'equal' to compare values."))

(defmethod initialize-instance :after ((mgr lru-set) &key capacity)
  (if (equal 0 capacity)
      (error "lru-set: capacity must be greater zero"))
  (let ((entry-array (make-array capacity)))
    (setf (slot-value mgr 'entries) entry-array)
    (dotimes (i capacity)
      (setf (aref entry-array i) (make-instance 'lru-entry :tick-counter (slot-value mgr 'tick-counter))))))

(defmacro inc-entry-count (cur-lru-set)
  `(setf (slot-value ,cur-lru-set 'entry-count) (+ 1 (slot-value ,cur-lru-set 'entry-count))))

(defmacro dec-entry-count (cur-lru-set)
  `(setf (slot-value ,cur-lru-set 'entry-count) (+ -1 (slot-value ,cur-lru-set 'entry-count))))

(defun lru-set-info (cur-lru-set value)
  "Returns values (index-set-eldest, index-nil-eldest, index-value, index-set-newest)"
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((entries (slot-value cur-lru-set 'entries))
	(min-tick-nil nil)
	(min-tick-set nil)
	(max-tick-set nil)
	(index-nil nil)
	(index-set nil)
	(index-set-cur nil)
	(index-value nil))
    ;;
    ;; Collect
    ;; - the eldest nil entry
    ;; - the eldest set entry
    ;; - index of the (optional) given value
    ;; - index of currently set value
    ;;
    (dotimes (index (length entries))
      (let ((entry (elt entries index)))
	(let ((tick (lru-entry-get-tick entry)) (cur-value (lru-entry-get-current-value entry)))
	  (if (and cur-value value (lru-entry-is-value entry value))
	      (setf index-value index))
	  (if (not cur-value)
	      (if (or (not min-tick-nil) (< tick min-tick-nil))
		  (progn
		    (setf min-tick-nil tick)
		    (setf index-nil index)))
	      ;; Entry value set
	      (progn
		(if (or (not max-tick-set) (< max-tick-set tick))
		    (progn
		      (setf index-set-cur index)
		      (setf max-tick-set tick)))
		(if (or (not min-tick-set) (< tick min-tick-set))
		    (progn
		      (setf min-tick-set tick)
		      (setf index-set index))))))))
    (values index-set index-nil index-value index-set-cur)))

(defun current-value (cur-lru-set)
  "Returns value of current (newest) set index or nil."
  (multiple-value-bind (index-set index-nil index-value index-set-cur)
      (lru-set-info cur-lru-set nil)
    (declare (ignore index-set index-nil index-value))
    (if (not index-set-cur)
	nil
	(lru-entry-get-current-value (elt (slot-value cur-lru-set 'entries) index-set-cur)))))

(defun push-value (cur-lru-set value)
  "Push and touch. Returns (values index stolen).
  - index Index of the set to which the value has been assigned or nil if value is nil.
  - stolen Boolean that indicates if insertion of the value has caused the removal of another value
    due to reaching the capacity limit of the set. "
  (if (not value)
      nil
      (multiple-value-bind (index-set index-nil index-value)
	  (lru-set-info cur-lru-set value)
	(cond
	  (index-value
	   (lru-entry-touch (elt (slot-value cur-lru-set 'entries) index-value))
	   (values index-value nil))
	  (index-nil
	   (inc-entry-count cur-lru-set)
	   (let ((entry (elt (slot-value cur-lru-set 'entries) index-nil)))
	     (lru-entry-set-value entry value)
	     (lru-entry-touch entry))
	   (values index-nil nil))
	  (index-set
	   (let ((entry (elt (slot-value cur-lru-set 'entries) index-set)))
	     (lru-entry-set-value entry value)
	     (lru-entry-touch entry))
	   (values index-set t))
	  (t (values nil nil))))))

(defun remove-value (cur-lru-set value)
  "Remove and touch. Returns an index or nil."
  (if (not value)
      nil
      (multiple-value-bind (index-set index-nil index-value)
	  (lru-set-info cur-lru-set value)
	(declare (ignore index-set index-nil))
	(if index-value
	    (progn
	      (dec-entry-count cur-lru-set)
	      (let ((entry (elt (slot-value cur-lru-set 'entries) index-value)))
		(lru-entry-set-value entry nil)
		(lru-entry-touch entry))))
	index-value)))

(defun get-value (cur-lru-set index)
  "Get a value by its index."
  (lru-entry-get-current-value (elt (slot-value cur-lru-set 'entries) index)))

(defun entry-count (cur-lru-set)
  "Get the current entry count."
  (slot-value cur-lru-set 'entry-count))

(defun empty-p (cur-lru-set)
  "Returns t if the set is empty."
  (= 0 (slot-value cur-lru-set 'entry-count)))

