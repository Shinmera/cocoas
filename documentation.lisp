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
    "")
  
  (function define-objcmethod
    "")
  
  (function with-objects
    "")
  
  (function with-foundation-objects
    ""))
