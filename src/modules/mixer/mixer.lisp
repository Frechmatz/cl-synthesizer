(in-package :cl-synthesizer-modules-mixer)

(defun make-symbol-impl (name num package)
  (if num
      (intern (format nil "~a-~a" (string-upcase name) num) package)
      (intern (string-upcase name) package)))

(defun make-keyword (name num)
  (make-symbol-impl name num "KEYWORD"))

(defun make-keyword-list (name count)
  "Returns list of keywords ordered by number of keyword: (:<name>-1, :<name>-2, ..., <name>-<count>.
   The numbering starts by one."
  (let ((l nil))
    (dotimes (i count)
      (push (make-keyword name (+ i 1)) l))
    (nreverse l)))

(defun make-module (name environment &key channel-count channel-cv-max channel-cv-gain
				 main-cv-max main-cv-gain)
  "Creates a mixer module. The mixer provides an attenuator for each input and a main
   attenuator for the mixer output. All attenuators have linear amplification
   characteristic. <p>The function has the following parameters:
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
    </ul></p>
    <p>The module has the following inputs:
    <ul>
        <li>:channel-1 ... :channel-n. Channel input signal, where n is the channel-count.</li>
        <li>:cv-1 ... :cv-n. Channel attenuation control voltage, where n is the channel-count.</li>
        <li>:cv-main Attenuation control voltage of the mixer output.</li> 
    </ul></p>
    <p>The module has the following outputs:
    <ul>
	<li>:output The output consisting of the sum of the inputs.</li>
    </ul></p>"
  (if (<= channel-count 0)
      (error
       'cl-synthesizer:assembly-error
       :format-control "~a: channel-count must be greater than 0: '~a'"
       :format-arguments (list name channel-count)))
  (let ((input-sockets
	 (concatenate
	  'list
	  (make-keyword-list "channel" channel-count)
	  (make-keyword-list "cv" channel-count)
	  (list :cv-main))))
    (let ((rack (cl-synthesizer:make-rack :environment environment)))

      ;; Add adder and main VCA and patch them
      (cl-synthesizer:add-module
       rack "ADDER"
       #'cl-synthesizer-modules-adder:make-module :input-count channel-count)
      (cl-synthesizer:add-module rack "MAIN-VCA" #'cl-synthesizer-modules-vca:make-module
				 :cv-max main-cv-max :initial-gain main-cv-gain)
      (cl-synthesizer:add-patch rack "ADDER" :output "MAIN-VCA" :input)
      (cl-synthesizer:expose-input-socket rack :cv-main "MAIN-VCA" :cv)
      (cl-synthesizer:expose-output-socket rack :output "MAIN-VCA" :output)
      
      ;; Add a VCA for each input and patch it with mixer input and adder
      (dotimes (i channel-count)
	(let ((vca-name (format nil "VCA-~a" (+ 1 i))))
	  (cl-synthesizer:add-module rack vca-name
				     #'cl-synthesizer-modules-vca:make-module
				     :cv-max channel-cv-max :initial-gain channel-cv-gain)
	  (cl-synthesizer:expose-input-socket rack 
				    (make-keyword "channel" (+ i 1))
				    vca-name :input)
	  (cl-synthesizer:expose-input-socket rack
				    (make-keyword "cv" (+ i 1))
				    vca-name :cv)
	  (cl-synthesizer:add-patch rack vca-name :output "ADDER"
				    (make-keyword "input" (+ i 1)))))
	  
      rack)))
