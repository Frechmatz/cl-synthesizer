# cl-synthesizer
An experimental synthesizer implemented in Common Lisp

Work In Progress...

## Example



## Api

A Synthesizer is represented by an instance of Rack. A rack holds instances of Rack-Modules. Each Rack-Module
is holding a Core-Module and the connections to other Rack-Modules.

### Core-Module

A function that returns the following alist

- :shutdown -- function to be called when synthesizer is shutting down
- :inputs -- function that returns a list of keywords representing the inputs of the module 
- :outputs -- function that returns a list of keywords representing the outputs of the module 
- :get-output(output) -- function that returns the value of the given output.
- :update(inputs) -- function to be called with all inputs as keyword parameters in order to update the internal state.
   Undefined inputs may be omitted.


todo: environment, ctor
### rack

-- (make-instance 'cl-synthesizer:rack :environment (cl-synthesizer::make-environment))))
--     (cl-synthesizer::add-module rack "Module 1" #'cl-synthesizer-test::test-module)


