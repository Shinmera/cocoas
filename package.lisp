(defpackage #:org.shirakumo.cocoas.cffi
  (:use #:cl)
  (:export
   #:cocoa
   #:appkit
   #:foundation
   #:id
   #:oclass
   #:sel
   #:cgfloat
   #:cfindex
   #:nsinteger
   #:string-encoding
   #:number-type
   #:release
   #:create-number
   #:number-get-value
   #:create-dictionary
   #:dictionary-contains-key
   #:dictionary-get-value
   #:dictionary-get-count
   #:dictionary-get-keys-and-values
   #:create-set
   #:set-get-count
   #:set-get-values
   #:create-array
   #:array-get-count
   #:array-get-value-at-index
   #:create-string-with-cstring
   #:string-get-length
   #:string-get-cstring
   #:string-get-cstring-ptr
   #:get-class
   #:register-name
   #:set-uncaught-exception-handler
   #:app
   #:event-mask
   #:default-run-loop-mode
   #:call
   #:free))

(defpackage #:org.shirakumo.cocoas
  (:use #:cl)
  (:local-nicknames
   (#:objc #:org.shirakumo.cocoas.cffi))
  (:export
   #:init
   #:shutdown
   #:define-objcfun
   #:define-objcmethod
   #:with-objects
   #:with-foundation-objects)
  (:export
   #:nsstring
   #:cfstring
   #:cfnumber
   #:cfset
   #:cfarray
   #:cfdictionary))
