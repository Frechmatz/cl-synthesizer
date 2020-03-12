(in-package :cl-synthesizer)

(defun make-environment (&key (sample-rate 44100) (home-directory nil))
  "Creates an environment. An enviroment is a property list with the following keys:
    <ul>
	<li>:sample-rate Sample rate of the synthesizer.</li>
	<li>:home-directory The base output directory for wave files etc. Default value is the home directory
        of the current user.</li>
    </ul>"
  (list
   :sample-rate (coerce sample-rate 'single-float)
   :home-directory (if (not home-directory) (user-homedir-pathname) home-directory) ))

