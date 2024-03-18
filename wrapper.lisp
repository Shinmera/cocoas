(in-package #:org.shirakumo.cocoas)

(defun init (&rest libs)
  (let ((libs (or libs '(:foundation :cocoa :appkit))))
    (unless (cffi:foreign-library-loaded-p 'objc:foundation)
      (cffi:load-foreign-library 'objc:foundation))
    (when (member :cocoa libs)
      (unless (cffi:foreign-library-loaded-p 'objc:cocoa)
        (cffi:load-foreign-library 'objc:cocoa)))
    (when (member :appkit libs)
      (unless (cffi:foreign-library-loaded-p 'objc:appkit)
        (cffi:load-foreign-library 'objc:appkit)))))

(defun shutdown ()
  )

(defmacro define-objcfun (class name rettype &body args)
  (destructuring-bind (name &optional method) (if (listp name) name (list name))
    (unless method
      (setf method (cffi:translate-camelcase-name name)))
    (etypecase class
      (symbol (setf class (cffi:translate-camelcase-name class :upper-initial-p T)))
      (string))
    `(defun ,name ,(loop for (name) in args collect name)
       (objc:call ,class ,method
                  ,@(loop for (name type) in args
                          collect type
                          collect name)
                  ,rettype))))

(defmacro define-objcmethod (name rettype &body args)
  (destructuring-bind (name &optional method) (if (listp name) name (list name))
    (unless method
      (setf method (cffi:translate-camelcase-name name)))
    (let ((self (gensym "SELF")))
      `(defun ,name (,self ,@(loop for (name) in args collect name))
         (objc:call ,self ,method
                    ,@(loop for (name type) in args
                            collect type
                            collect name)
                    ,rettype)))))

(defmacro with-objects (bindings &body body)
  `(let ,bindings
     (unwind-protect
          (let ,(loop for (name) in strings collect `(,name ,name))
            ,@body)
       ,@(loop for (name) in bindings
               collect `(free ,name)))))

(cffi:define-foreign-type nsstring ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method nsstring ()
  (load-time-value (make-instance 'nsstring)))

(defmethod cffi:translate-to-foreign ((string string) (type nsstring))
  (values (objc:call "NSString" "stringWithUTF8String:" :string string) T))

(defmethod cffi:translate-to-foreign (obj (type nsstring))
  (if (cffi:pointerp obj)
      (values obj NIL)
      (error "~a is neither a string nor pointer." obj)))

(defmethod cffi:translate-from-foreign (ptr (type nsstring))
  (unwind-protect (objc:call "NSString" "UTF8String" :string)
    (objc:free ptr)))

(defmethod cffi:free-translated-object (ptr (type nsstring) free-p)
  (when free-p
    (objc:free ptr)))

(cffi:define-foreign-type cfstring ()
  ()
  (:actual-type :pointer))

(cffi:define-parse-method cfstring ()
  (load-time-value (make-instance 'cfstring)))

(defmethod cffi:translate-to-foreign ((string string) (type cfstring))
  (values (string->cfstring string) T))

(defmethod cffi:translate-to-foreign (obj (type cfstring))
  (if (cffi:pointerp obj)
      (values obj NIL)
      (error "~a is neither a string nor pointer." obj)))

(defmethod cffi:translate-from-foreign (ptr (type cfstring))
  (unwind-protect (cfstring->string ptr)
    (objc:release ptr)))

(defmethod cffi:free-translated-object (ptr (type cfstring) free-p)
  (when free-p
    (objc:release ptr)))

(defun cfstring->string (pointer)
  (let ((buffer (objc:string-get-cstring-ptr pointer :utf-8)))
    (cond ((cffi:null-pointer-p buffer)
           (let ((length (1+ (* 2 (objc:string-get-length pointer)))))
             (if (= 0 length)
                 (make-string 0)
                 (cffi:with-foreign-object (buffer :uint8 length)
                   (if (objc:string-get-cstring pointer buffer length :utf-8)
                       (cffi:foreign-string-to-lisp buffer :encoding :utf-8)
                       (error "Failed to convert string to lisp!"))))))
          (T
           (cffi:foreign-string-to-lisp buffer :encoding :utf-8)))))

(defun string->cfstring (string)
  (cffi:with-foreign-string (buffer string :encoding :utf-8)
    (objc:create-string-with-cstring (cffi:null-pointer) buffer :utf-8 NIL)))
