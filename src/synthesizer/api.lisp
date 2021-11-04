(in-package :cl-synthesizer)

(defun get-inputs-fn (module)
  (getf module :inputs))

(defun get-inputs (module)
  (funcall (getf module :inputs)))

(defun get-outputs-fn (module)
  (getf module :outputs))

(defun get-outputs (module)
  (funcall (getf module :outputs)))

(defun get-update-fn (module)
  (getf module :update))

(defun update (module)
  (funcall (getf module :update)))

(defun get-state-fn (module)
  (getf module :state))

(defun get-state (module key)
  (let ((fn (get-state-fn module)))
    (if fn
	(funcall fn key)
	nil)))

(defun get-shutdown-fn (module)
  (getf module :shutdown))

(defun shutdown (module)
  (let ((fn (get-shutdown-fn module)))
    (if fn (funcall fn))))

(defun get-modules-fn (rack)
  (getf rack :modules))

(defun get-modules (rack)
  "Get all modules of a rack. <p>The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
    </ul></p>
    Returns a list of modules where each module consists of a property list with
    the following keys:
    <ul>
       <li>:module The module</li>
       <li>:name Name of the module</li>
    </ul>"
  (funcall (getf rack :modules)))

(defun get-patches-fn (rack)
  (getf rack :patches))

(defun get-patches (rack)
  "Get all patches of a rack. <p>The function has the following arguments:
    <ul>
	<li>rack The rack.</li>
    </ul></p>
   Returns a list of property lists with the following keys:
   <ul>
     <li>:output-name Name of the output module.</li>
     <li>:output-socket Output socket. </li>
     <li>:input-name Name of the input module. </li>
     <li>:input-socket Input socket.</li>
   </ul>"
  (funcall (getf rack :patches)))

(defun is-rack (module)
  "Returns <b>t</b> if the given module represents a rack."
  (getf module :is-rack))

