(in-package :cl-synthesizer)

(define-condition assembly-error (simple-error) ()
  (:documentation
   "This condition is signalled in cases where the assembly of a rack fails,
   because for example a module name is not unique, a module is malformed, a patch is added for
   a non-existing module, a patch is added to an already patched socket and so on."))

