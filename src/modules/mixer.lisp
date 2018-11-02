(in-package :cl-synthesizer-modules-mixer)

(defparameter *mixer-input-channel* "channel")
(defparameter *mixer-input-cv* "cv")
(defparameter *mixer-input-cv-main* :cv-main)

(defun mixer (name environment &key channel-count (channel-cv-max 5.0) (channel-cv-gain 5.0)
				 (main-cv-max 5.0) (main-cv-gain 5.0))
  "Creates a mixer module. The mixer provides an attenuator for each input and a main
   attenuator for the mixer output. All attenuators have linear amplification
   characteristic. The function has the following arguments:
    <ul>
	<li>name Name of the module.</li>
	<li>environment The synthesizer environment.</li>
	<li>:channel-count The number of channels.</li>
        <li>:channel-cv-max The value of a channel attenuation control voltage that 
        represents an amplification of 1.0.</li>
        <li>:channel-cv-gain An offset that is added to the channel attenuation 
        control voltage.</li>
        <li>:main-cv-max The value of the main attenuation input that 
        represents an amplification of 1.0.</li>
        <li>:main-cv-gain An offset that is added to the main attenuation 
        control voltage.</li>
    </ul>
    The module has the following inputs:
    <ul>
        <li>:channel-1 ... :channel-n. Channel input signal, where n is the channel-count.</li>
        <li>:cv-1 ... :cv-n. Channel attenuation control voltage, where n is the channel-count.</li>
        <li>:cv-main Attenuation control voltage of the mixer output.</li> 
    </ul>
    The module has the following outputs:
    <ul>
	<li>:output The output consisting of the sum of the inputs.</li>
    </ul>"
  (if (<= channel-count 0)
      (cl-synthesizer:signal-assembly-error
       :format-control "~a: channel-count must be greater than 0: ~a"
       :format-arguments (list name channel-count)))
  (let ((input-sockets
	 (concatenate
	  'list
	  (cl-synthesizer-macro-util:make-keyword-list *mixer-input-channel* channel-count)
	  (cl-synthesizer-macro-util:make-keyword-list *mixer-input-cv* channel-count)
	  (list *mixer-input-cv-main*))))
    (let ((rack (cl-synthesizer:make-rack
		 :environment environment
		 :input-sockets input-sockets
		 :output-sockets '(:output))))

      ;; Add adder and main VCA and patch them
      (cl-synthesizer:add-module
       rack "ADDER"
       #'cl-synthesizer-modules-adder:adder :input-count channel-count)
      (cl-synthesizer:add-module rack "MAIN-VCA" #'cl-synthesizer-modules-vca:vca
				 :cv-max main-cv-max :initial-gain main-cv-gain)
      (cl-synthesizer:add-patch rack "ADDER" :output "MAIN-VCA" :input)
      (cl-synthesizer:add-patch rack "INPUT" *mixer-input-cv-main* "MAIN-VCA" :cv)
      (cl-synthesizer:add-patch rack "MAIN-VCA" :output-linear "OUTPUT" :output)
      
      ;; Add a VCA for each input and patch it with mixer input and adder
      (dotimes (i channel-count)
	(let ((vca-name (format nil "VCA-~a" (+ 1 i))))
	  (cl-synthesizer:add-module rack vca-name
				     #'cl-synthesizer-modules-vca:vca
				     :cv-max channel-cv-max :initial-gain channel-cv-gain)
	  (cl-synthesizer:add-patch rack "INPUT"
				    (cl-synthesizer-macro-util:make-keyword *mixer-input-channel* i)
				    vca-name :input)
	  (cl-synthesizer:add-patch rack "INPUT"
				    (cl-synthesizer-macro-util:make-keyword *mixer-input-cv* i)
				    vca-name :cv)
	  (cl-synthesizer:add-patch rack vca-name :output-linear "ADDER"
				    (cl-synthesizer-macro-util:make-keyword "input" i))))
	  
      rack)))
