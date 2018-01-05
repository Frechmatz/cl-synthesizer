# cl-synthesizer
An experimental modular audio synthesizer implemented in Common Lisp

Work In Progress...

## Architecture

A Synthesizer is represented by an instance of Rack. A rack holds instances of Rack-Modules. Each Rack-Module
holds a Core-Module and connections to other Rack-Modules.

### Core-Module

A function that returns an alist with the following properties:

- :shutdown -- function that is called when the synthesizer is shutting down.
- :inputs -- function that returns a list of keywords representing the inputs of the module. 
- :outputs -- function that returns a list of keywords representing the outputs of the module. 
- :get-output(output) -- function that is called to get the current value of the given output.
- :update(inputs) -- function that is called with all inputs (as declared by the inputs function) as keyword parameters in order to update the state of the module. Undefined (not connected) inputs are not passed. It is up to the module to define default values for unconnected inputs. 

### Rack

### Rack-Module

