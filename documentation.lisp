(in-package #:org.shirakumo.cocoas)

(docs:define-docs
  (type nsstring
    "CFFI type for NSString translation.")

  (type cfstring
    "CFFI type for CFString translation.")

  (type cfnumber
    "CFFI type for CFNumber translation.

Requires an argument that specifies the base number type to translate
between:

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
number type.")

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

RETTYPE should be the return type of the function.

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

RETTYPE should be the return type of the function.

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
ERROR form is generated for you."))
