(in-package #:org.shirakumo.cocoas)

(docs:define-docs
  (type id
    "CFFI type for Objective C IDs.")

  (type oclass
    "CFFI type for Objective C Classes.")

  (type sel
    "CFFI type for Objective C method names/SELs.")

  (type cgfloat
    "CFFI type for CGFloat.")

  (type cfindex
    "CFFI type for CFIndex.")

  (type nsinteger
    "CFFI type for NSInteger.")

  (function release
    "Function to call Objective C's release on an object.")
  
  (function free
    "Function to call CoreFoundation's dealloc on an object.")
  
  (variable app
    "Accesses the NSApp instance.")
  
  (function call
    "Macro to perform an Objective C call.

SELF must either be a string denoting a class name or a pointer to an
Objective C instance. METHOD must be a string naming the method to
call.

ARGS should be a list of alternating argument types and argument
values, and optionally the final return type, same as
CFFI:FOREIGN-FUNCALL. If the return type is not specified, it is
defaulted to ID.

The class and method name resolution happen once on first execute and
are then cached at the call-site."))

(docs:define-docs
  (type nsstring
    "CFFI type for NSString translation.")

  (type cfstring
    "CFFI type for CFString translation.")

  (type cfnumber
    "CFFI type for CFNumber translation.

An optional argument may specify the raw number type to use, which may
be one of the following:

  :int8
  :int16
  :int32
  :int64
  :char
  :short
  :int
  :long
  :longlong
  :float
  :double
  org.shirakumo.cocoas.cffi:cfindex
  org.shirakumo.cocoas.cffi:nsinteger
  org.shirakumo.cocoas.cffi:cgfloat

The actual lisp value will be coerced (as possible) to conform to the
number type. If not specified, the default is
org.shirakumo.cocoas.cffi:cgfloat")

  (type cfset
    "CFFI type for CFSet translation.

An optional argument may specify how to translate the individual
pointer values of the set. Defaults to just :pointer.

Will translate to a list and from a list or vector.")

  (type cfarray
    "CFFI type for CFSet translation.

An optional argument may specify how to translate the individual
pointer values of the array. Defaults to just :pointer.

Will translate to a vector and from a list or vector.")

  (type cfdictionary
    "CFFI type for CFDictionary translation.

Two optional arguments may specify how to translate the individual
pointer values of the dictionary keys and values respectively. 
Defaults to just :pointer for both.

Will translate to a hash-table with an EQUAL test, and from a
hash-table or alist."))

(docs:define-docs
  (type foundation-error
    "Representation of an NSException error.

Signalled for uncaught NSExceptions.

See NAME
See REASON")

  (function name
    "Returns the name for the foundation-error.

See FOUNDATION-ERROR (type)")

  (function reason
    "Returns the reason for the foundation-error, if any.

See FOUNDATION-ERROR (type)")
  
  (function init
    "Initialises the libraries.

LIBS can be a set containing:
  :FOUNDATION
  :COCOA
  :APPKIT

If LIBS is not specified, all libraries are loaded.

See SHUTDOWN")
  
  (function shutdown
    "Cleans up the libraries.

See INIT")
  
  (function define-objcfun
    "Defines an ObjC static class method/function.

CLASS should be the class name the method is defined on.

NAME should be the name of the method. It can be a list of the
lisp-side function name and the method name. If the method name is not
explicitly given, it is translated from the lisp name. To specify
colons in the name, you may use a slash instead. For instance, the
lisp-name

   foo-bar/baz

is translated to

   fooBar:baz

If the NAME is not a list, the class is automatically prepended to the
name of the resulting lisp function name, meaning

   (define-objcfun \"foo\" bar ...)

Results in the defined lisp function being named FOO-BAR, but

   (define-objcfun \"foo\" (bar) ...)

Results in the defined lisp function being named BAR.

RETTYPE should be the return type of the function. It can also be NIL,
in which case it is automatically replaced with OBJC:ID.

ARGS should be a body of list specifying the function arguments, with
the argument name first and the type second, same as CFFI:DEFCFUN.

See CFFI:DEFCFUN
See DEFINE-OBJCMETHOD")
  
  (function define-objcmethod
    "Defines an ObjC instance method/function.

NAME should be the name of the method. It can be a list of the
lisp-side function name and the method name. If the method name is not
explicitly given, it is translated from the lisp name. To specify
colons in the name, you may use a slash instead. For instance, the
lisp-name

   foo-bar/baz

is translated to

   fooBar:baz

RETTYPE should be the return type of the function. It can also be NIL,
in which case it is automatically replaced with OBJC:ID.

ARGS should be a body of list specifying the function arguments, with
the argument name first and the type second, same as CFFI:DEFCFUN.

See CFFI:DEFCFUN
See DEFINE-OBJCFUN")
  
  (function with-objects
    "Bind a bunch of ObjC objects.

Automatically calls OBJC:FREE to deallocate the objects on exit. If an
object is bound to a null pointer, its failure form is returned.

Each binding may be a list of the variable name, initialiser form, and
an optional failure form. If the failure form is not specified, an
ERROR form is generated for you.")
  
  (function with-foundation-objects
    "Bind a bunch of CoreFramework objects.

Automatically calls OBJC:RELEASE to deallocate the objects on exit. If an
object is bound to a null pointer, its failure form is returned.

Each binding may be a list of the variable name, initialiser form, and
an optional failure form. If the failure form is not specified, an
ERROR form is generated for you.")

  (function process-event
    "Process an event from the apps' queue.

Returns true if an event was processed and NIL otherwise.")

  (function with-main-loop
    "Runs BODY in the main thread.

MacOS requires GUI things to run on the main thread. Additionally,
system API calls frequently generate floating point traps which your
Lisp implementation will likely intercept. This also disables those to
ensure your application can run properly.

See FLOAT-FEATURES:WITH-FLOAT-TRAPS-MASKED
See TRIVIAL-MAIN-THREAD:WITH-BODY-IN-MAIN-THREAD"))
