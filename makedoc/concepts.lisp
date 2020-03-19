(in-package :cl-synthesizer-makedoc)

(defun get-concepts ()
  `((heading (:name "Environment" :toc t)
"<p>An environment defines properties such as the sample rate and the home directory of the synthesizer. An environment is represented by a property list.</p>"
,(cl-readme:read-code "makedoc/snippets/environment-make-environment.lisp"))
    (heading (:name "Module" :toc t)
"<p>Modules are the heart of cl-synthesizer because they are responsible for producing the
actual audio data.</p>
<p>Modules do have a name, inputs, outputs and a shutdown function. The inputs/outputs are represented by keywords and are so called sockets. The shutdown function can be used to release resources 
that have been allocated by the module, for example opened files.</p>
<p>Beside the input/output sockets a module can also provide \"state sockets\".
State sockets expose internal states of the module. These sockets 
are not accessible when connecting modules with each other. They are meant 
to be a debugging/analysis tool. For example to create a plot of the phase of an 
oscillator over time, a :phase state socket in conjunction with a Monitor is the way to go.</p>
<p>A module is represented by a property list. This list provides functions such as
to get the input sockets, to get the output sockets, to get the state sockets, 
to set input values, to retrieve output values, to update the module, to
shutdown the module and so on.</p>
<p>A module must provide a factory/instantiation function. The typical name of this 
    function is ’\"make-module\". When a module is added to the synthesizer then not the 
    readily instantiated module (as a property list) is passed, but its factory function. 
    This function is called by the synthesizer. The synthesizer passes the module name, 
    the environment and any arbitrary initialization parameters to it.</p>
<p>A module can implement all its logic on its own but it can also use
    other modules. An example of a module using other modules is the
    <a href=\"https://github.com/Frechmatz/cl-synthesizer/blob/master/src/modules/mixer.lisp\">Mixer</a></p>
<p>For each input/output socket that a module exposes, it must provide a corresponding setter/getter function. When processing an update, the synthesizer sets the inputs of the module via successive calls to the input setters. An input setter must not change the current output state of the module. When all inputs have been set, the synthesizer calls the update function of the module, which has no arguments. The update function must update the states of the output sockets, by using the previously set input values.</p>
<p>Lets create a module:</p>"
,(cl-readme:read-code "makedoc/snippets/module-blueprint.lisp")
"<p>Now lets add our module to a rack (Racks are explained in the following chapter):</p>"
,(cl-readme:read-code "makedoc/snippets/module-blueprint-add.lisp"))
    (heading (:name "Rack" :toc t)
"<p>Racks contain modules and their connections which each other. The connections are so called \"Patches\". A rack is represented by a property list, which is fully compatible with a module. Beside the module functionality a rack provides Module and Patch management, Hooks and Compilation. Racks are instantiated via \"cl-synthesizer:make-rack\"</p>"
,(cl-readme:read-code "makedoc/snippets/rack-make-rack.lisp")
"<p>After the rack has been created, modules and patches can be added to it.</p>"
,(cl-readme:read-code "makedoc/snippets/rack-add-modules.lisp")
"<p>The rack is now ready to use and a \"tick\" can be processed. When a tick is to be processed, the rack checks if it has been modified, and if yes, a compilation occurs. The compilation step processes the modules and patch table of the rack in order to eliminate/minimize any expensive property list lookups during the actual execution.</p>"
,(cl-readme:read-code "makedoc/snippets/rack-process-tick.lisp")
"<p>Lets get the :sine output of the \"VCO\" module. The following
snippet illustrates the low-level Api of the synthesizer. The recommended way to
retrieve output values is to use a \"Monitor\”. Monitors are explained in the next chapter.</p>"
,(cl-readme:read-code "makedoc/snippets/rack-get-output.lisp")
"<p>When all things are done the rack should be shut down in order to release any resources that may have been allocated, such as opened files.</p>"
,(cl-readme:read-code "makedoc/snippets/rack-shutdown.lisp")
"<p>Racks may expose input and output sockets. When a rack exposes sockets it can be used like any other module when populating another rack. The inputs and outputs are accessible by the internal patching of the rack via so called \"Bridge modules\". \"INPUT\" provides the inputs, and \"OUTPUT\" the outputs of the rack.</p>"
,(cl-readme:read-code "makedoc/snippets/rack-expose-sockets.lisp")
"<p>A more comprehensive example can be found <a href=\"https://github.com/Frechmatz/cl-synthesizer/blob/master/patches/siren.lisp\">here</a>.</p>")
    (heading (:name "Monitor" :toc t)
"<p>Monitors are high-level Rack-Hooks. The purpose of a monitor
is to collect module states and pass them to a \"Monitor-Backend\". A backend may
for example generate a Wave file.</p> 
<p>Monitors provide a simple syntax for declaring the module sockets to be tracked (input, output and state) as well as any other settings supported by specific backends.</p>
<p>A backend is typically realized as a plain module, which declares
input sockets, output sockets, a shutdown function and so on. Backends are 
instantiated by a so called \"Monitor-Handler\".</p>
<p>A Monitor-Handler is a factory function whose purpose is to prepare the initialization 
parameters of a specific backend (e.g. a Wave-File-Writer module), to set up the 
mapping of the values collected by the monitor to input sockets of the backend 
and finally to instantiate it.</p>"
,(make-example-header)
,(cl-readme:read-code "src/monitor/monitor/example-1.lisp"))))
