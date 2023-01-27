(in-package :cl-synthesizer-test)


(defun get-module-input-patch(rack module input-socket)
  (let* ((name (cl-synthesizer:get-module-name rack module))
	 (patch (find-if
		 (lambda (p)
		   (and (string= name (cl-synthesizer:get-patch-input-name p))
			(eq input-socket (cl-synthesizer:get-patch-input-socket p))))
		 (cl-synthesizer:get-patches rack))))
	 patch))

(defun get-module-output-patch (rack module output-socket)
  (let* ((name (cl-synthesizer:get-module-name rack module))
	 (patch (find-if
		 (lambda (p)
		   (and (string= name (cl-synthesizer:get-patch-output-name p))
			(eq output-socket (cl-synthesizer:get-patch-output-socket p))))
		 (cl-synthesizer:get-patches rack))))
    patch))

(defun get-patch (rack module-name socket-type socket)
  "Returns the destination module and input/output socket, to which a given
    source module and one if its input/output sockets is connected.
    The function has the following parameters:
    <ul>
	<li>rack The rack.</li>
	<li>module-name Name of the source module.</li>
	<li>socket-type :input-socket if the patch of an input socket is required or
	    :output-socket for the patch of an output socket of the source module.</li>
	<li>socket A keyword identifying an input or output socket of the source module.</li>
    </ul>
    The function returns returns a values object with the following entries:
    <ul>
	<li>name Name of the destination module.</li>
	<li>module The destination module represented as a property list.</li>
	<li>socket A keyword that identifies the input or output socket of the destination
	    module. If the socket type of the source module is :input-socket then this
	    keyword represents an output socket of the destination module. Otherwise
	    it represents an input socket.
	</li>
    </ul>
    If the module does not exist, the module does not expose the given socket, or
    if the socket is not patched, all entries of the returned values object are nil."
  (if (not (or (eq :input-socket socket-type) (eq :output-socket socket-type)))
      (error (make-condition
	      'simple-error
	      :format-control "get-patch: socket must be one of :input-socket or :output-socket"
	      :format-arguments nil)))
  (let ((rm (cl-synthesizer:get-module rack module-name)))
    (if (not rm)
	(values nil nil nil)
	(if (eq :input-socket socket-type)
	    (let ((patch (get-module-input-patch rack rm socket)))
	      (if (not patch)
		  (values nil nil nil)
		  (values
		   (cl-synthesizer:get-patch-output-name patch)
		   (cl-synthesizer:get-module rack (cl-synthesizer:get-patch-output-name patch))
		   (cl-synthesizer:get-patch-output-socket patch))))
	    (let ((patch (get-module-output-patch rack rm socket)))
	      (if (not patch)
		  (values nil nil nil)
		  (values
		   (cl-synthesizer:get-patch-input-name patch)
		   (cl-synthesizer:get-module rack (cl-synthesizer:get-patch-input-name patch))
		   (cl-synthesizer:get-patch-input-socket patch))))))))
