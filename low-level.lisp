(in-package #:org.shirakumo.cocoas.cffi)

(cffi:define-foreign-library cocoa
  (T (:framework "Cocoa")))

(cffi:define-foreign-library appkit
  (T (:framework "AppKit")))

(cffi:define-foreign-library foundation
  (T (:framework "Foundation")))

(cffi:defctype id :pointer)
(cffi:defctype oclass :pointer)
(cffi:defctype sel :pointer)
(cffi:defctype cgfloat :double)
(cffi:defctype cfindex :long)
(cffi:defctype nsinteger :long)

(cffi:defcenum (string-encoding :uint32)
  (:utf-8 #x08000100))

(cffi:defcenum (number-type cfindex)
  (:int8 1)
  (:int16 2)
  (:int32 3)
  (:int64 4)
  (:char 7)
  (:short 8)
  (:int 9)
  (:long 10)
  (:longlong 11)
  (:float 12)
  (:double 13)
  (cfindex 14)
  (nsinteger 15)
  (cgfloat 16))

(cffi:defcfun (release "CFRelease") :void
  (object :pointer))

(cffi:defcfun (create-number "CFNumberCreate") :pointer
  (allocator :pointer)
  (type number-type)
  (value :pointer))

(cffi:defcfun (number-get-value "CFNumberGetValue") :bool
  (number :pointer)
  (type number-type)
  (value :pointer))

(cffi:defcfun (create-dictionary "CFDictionaryCreate") :pointer
  (allocator :pointer)
  (keys :pointer)
  (values :pointer)
  (num-values cfindex)
  (key-callbacks :pointer)
  (value-callbacks :pointer))

(cffi:defcfun (dictionary-contains-key "CFDictionaryContainsKey") :pointer
  (dictionary :pointer)
  (key :pointer))

(cffi:defcfun (dictionary-get-value "CFDictionaryGetValue") :pointer
  (dictionary :pointer)
  (key :pointer))

(cffi:defcfun (dictionary-get-count "CFDictionaryGetCount") cfindex
  (dictionary :pointer))

(cffi:defcfun (dictionary-get-keys-and-values "CFDictionaryGetKeysAndValues") :void
  (dictionary :pointer)
  (keys :pointer)
  (values :pointer))

(cffi:defcfun (create-set "CFSetCreate") :pointer
  (allocator :pointer)
  (values :pointer)
  (num-values cfindex)
  (callbacks :pointer))

(cffi:defcfun (set-get-count "CFSetGetCount") cfindex
  (set :pointer))

(cffi:defcfun (set-get-values "CFSetGetValues") :void
  (set :pointer)
  (values :pointer))

(cffi:defcfun (create-array "CFArrayCreate") :pointer
  (allocator :pointer)
  (values :pointer)
  (num-values cfindex)
  (callbacks :pointer))

(cffi:defcfun (array-get-count "CFArrayGetCount") cfindex
  (array :pointer))

(cffi:defcfun (array-get-value-at-index "CFArrayGetValueAtIndex") :pointer
  (array :pointer)
  (index cfindex))

(cffi:defcfun (create-string-with-cstring "CFStringCreateWithCString") :pointer
  (allocator :pointer)
  (bytes :pointer)
  (encoding string-encoding)
  (is-external-representation :bool))

(cffi:defcfun (string-get-length "CFStringGetLength") cfindex
  (string :pointer))

(cffi:defcfun (string-get-cstring "CFStringGetCString") :bool
  (string :pointer)
  (buffer :pointer)
  (length cfindex)
  (encoding string-encoding))

(cffi:defcfun (string-get-cstring-ptr "CFStringGetCStringPtr") :pointer
  (string :pointer)
  (encoding string-encoding))

(cffi:defcfun (get-class "objc_getClass") oclass
  (name :string))

(cffi:defcfun (register-name "sel_registerName") sel
  (name :string))

(cffi:defcfun (set-uncaught-exception-handler "NSSetUncaughtExceptionHandler") :void
  (handler :pointer))

(cffi:defcallback exception-handler :void ((object id) (pointer :pointer))
  (error "Fuck!"))

(cffi:defcvar (app "NSApp") :pointer)

(cffi:defcenum (event-mask :uint64)
  (:any #.(1- (ash 1 64))))

(cffi:defcvar (default-run-loop-mode "NSDefaultRunLoopMode") :pointer)

(defmacro %cache (value)
  (let ((cache (gensym "CACHE")))
    `(let ((,cache (cons NIL NIL)))
       (or (car ,cache) (setf (car ,cache) ,value)))))

(defmacro call (self method &rest args)
  (when (stringp self)
    (setf self `(%cache (get-class ,self))))
  (when (evenp (length args))
    (setf args (append args '(id))))
  (let* ((struct (gensym "STRUCT"))
         (retval (car (last args)))
         (base-args (list* 'id self
                           'sel `(%cache (register-name ,method))
                           (loop for (type name) on (butlast args) by #'cddr
                                 collect `,type collect name))))
    (cond ((and (listp retval) (eq :struct (first retval)))
           `(cffi:with-foreign-object (,struct ',retval)
              (cffi:foreign-funcall "objc_msgSend_stret" :pointer ,struct ,@base-args :void)
              (cffi:mem-ref ,struct ',retval)))
          ((find retval '(:double :float))
           `(cffi:foreign-funcall "objc_msgSend_fpret" ,@base-args ,retval))
          (T
           `(cffi:foreign-funcall "objc_msgSend" ,@base-args ,retval)))))

(defun free (id)
  (call id "dealloc" :void))
