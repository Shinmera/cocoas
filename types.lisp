(in-package #:org.shirakumo.cocoas)

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
  (unless (cffi:null-pointer-p ptr)
    (unwind-protect (objc:call "NSString" "UTF8String" :string)
      (objc:free ptr))))

(defmethod cffi:free-translated-object (ptr (type nsstring) free-p)
  (when free-p
    (objc:free ptr)))

(cffi:define-foreign-type cftype ()
  ()
  (:actual-type :pointer))

(defmethod cffi:translate-to-foreign (obj (type cftype))
  (if (cffi:pointerp obj)
      (values obj NIL)
      (error "~a is not a translatable type nor pointer." obj)))

(defmethod cffi:translate-from-foreign :around (ptr (type cftype))
  (unwind-protect (call-next-method)
    (objc:release ptr)))

(defmethod cffi:free-translated-object (ptr (type cftype) free-p)
  (when free-p
    (objc:release ptr)))

(cffi:define-foreign-type cfstring (cftype)
  ()
  (:actual-type :pointer))

(cffi:define-parse-method cfstring ()
  (load-time-value (make-instance 'cfstring)))

(defmethod cffi:translate-to-foreign ((string string) (type cfstring))
  (values (string->cfstring string) T))

(defmethod cffi:translate-from-foreign (ptr (type cfstring))
  (cfstring->string ptr))

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

(cffi:define-foreign-type cfnumber (cftype)
  ((number-type :initarg :number-type :reader number-type))
  (:actual-type :pointer))

(cffi:define-parse-method cfnumber (&optional (number-type 'objc:cgfloat))
  (unless (member number-type (cffi:foreign-enum-keyword-list 'objc:number-type))
    (error "~s is not a valid number type." number-type))
  (make-instance 'cfnumber :number-type number-type))

(defmethod cffi:translate-to-foreign ((number real) (type cfnumber))
  (let ((type (number-type type)))
    (values (cffi:with-foreign-objects ((value type))
              (setf (cffi:mem-ref value type)
                    (case type
                      (:float (float number 0f0))
                      (:double (float number 0d0))
                      (objc:cgfloat (float number 0d0))
                      (T (truncate number))))
              (objc:create-number (cffi:null-pointer) type value))
            T)))

(defmethod cffi:translate-from-foreign (ptr (type cfnumber))
  (let ((type (number-type type)))
    (cffi:with-foreign-objects ((value type))
      (unless (objc:number-get-value ptr type value)
        (error "Failed to retrieve numerical value."))
      (cffi:mem-ref value type))))

(cffi:define-foreign-type cfset ()
  ((internal-type :initarg :internal-type :reader internal-type))
  (:actual-type :pointer))

(cffi:define-parse-method cfset (&optional (internal-type :pointer))
  (make-instance 'cfset :internal-type internal-type))

(defmethod cffi:translate-to-foreign ((list list) (type cfset))
  (let ((type (internal-type type)))
    (values (cffi:with-foreign-objects ((values :pointer (length list)))
              (loop for i from 0
                    for value in list
                    do (setf (cffi:mem-aref values :pointer i)
                             (cffi:translate-to-foreign value type)))
              (objc:create-set (cffi:null-pointer) values (length list) (cffi:null-pointer)))
            T)))

(defmethod cffi:translate-to-foreign ((list vector) (type cfset))
  (let ((type (internal-type type)))
    (values (cffi:with-foreign-objects ((values :pointer (length list)))
              (loop for i from 0
                    for value across list
                    do (setf (cffi:mem-aref values :pointer i)
                             (cffi:translate-to-foreign value type)))
              (objc:create-set (cffi:null-pointer) values (length list) (cffi:null-pointer)))
            T)))

(defmethod cffi:translate-from-foreign (ptr (type cfset))
  (let ((type (internal-type type))
        (count (objc:set-get-count ptr)))
    (cffi:with-foreign-objects ((values :pointer count))
      (objc:set-get-values ptr values)
      (loop for i from 0 below count
            collect (cffi:translate-from-foreign (cffi:mem-aref values :pointer i) type)))))

(cffi:define-foreign-type cfarray ()
  ((internal-type :initarg :internal-type :reader internal-type))
  (:actual-type :pointer))

(cffi:define-parse-method cfarray (&optional (internal-type :pointer))
  (make-instance 'cfarray :internal-type internal-type))

(defmethod cffi:translate-to-foreign ((list list) (type cfarray))
  (let ((type (internal-type type)))
    (values (cffi:with-foreign-objects ((values :pointer (length list)))
              (loop for i from 0
                    for value in list
                    do (setf (cffi:mem-aref values :pointer i)
                             (cffi:translate-to-foreign value type)))
              (objc:create-array (cffi:null-pointer) values (length list) (cffi:null-pointer)))
            T)))

(defmethod cffi:translate-to-foreign ((list vector) (type cfarray))
  (let ((type (internal-type type)))
    (values (cffi:with-foreign-objects ((values :pointer (length list)))
              (loop for i from 0
                    for value across list
                    do (setf (cffi:mem-aref values :pointer i)
                             (cffi:translate-to-foreign value type)))
              (objc:create-array (cffi:null-pointer) values (length list) (cffi:null-pointer)))
            T)))

(defmethod cffi:translate-from-foreign (ptr (type cfarray))
  (let ((type (internal-type type))
        (array (make-array (objc:array-get-count ptr))))
    (dotimes (i (length array) array)
      (setf (aref array i) (cffi:translate-from-foreign (objc:array-get-value-at-index ptr i) type)))))

(cffi:define-foreign-type cfdictionary ()
  ((internal-type :initarg :internal-type :reader internal-type))
  (:actual-type :pointer))

(cffi:define-parse-method cfdictionary (&optional (key-type :pointer) (value-type :pointer))
  (make-instance 'cfdictionary :internal-type (cons key-type value-type)))

(defmethod cffi:translate-to-foreign ((list list) (type cfdictionary))
  (destructuring-bind (key-type . value-type) (internal-type type)
    (let ((length (length list)))
      (values (cffi:with-foreign-objects ((keys :pointer length)
                                          (values :pointer length))
                (loop for i from 0
                      for (key . value) in list
                      do (setf (cffi:mem-aref keys :pointer i) (cffi:translate-to-foreign value key-type))
                         (setf (cffi:mem-aref values :pointer i) (cffi:translate-to-foreign value value-type)))
                (objc:create-dictionary (cffi:null-pointer) keys values length (cffi:null-pointer) (cffi:null-pointer)))
              T))))

(defmethod cffi:translate-to-foreign ((table hash-table) (type cfdictionary))
  (destructuring-bind (key-type . value-type) (internal-type type)
    (let ((length (hash-table-count table)))
      (values (cffi:with-foreign-objects ((keys :pointer length)
                                          (values :pointer length))
                (loop for i from 0
                      for key being the hash-keys of table using (hash-value value)
                      do (setf (cffi:mem-aref keys :pointer i) (cffi:translate-to-foreign value key-type))
                         (setf (cffi:mem-aref values :pointer i) (cffi:translate-to-foreign value value-type)))
                (objc:create-dictionary (cffi:null-pointer) keys values length (cffi:null-pointer) (cffi:null-pointer)))
              T))))

(defmethod cffi:translate-from-foreign (ptr (type cfdictionary))
  (destructuring-bind (key-type . value-type) (internal-type type)
    (let* ((length (objc:dictionary-get-count ptr))
           (table (make-hash-table :test 'equal :size length)))
      (cffi:with-foreign-objects ((keys :pointer length)
                                  (values :pointer length))
        (objc:dictionary-get-keys-and-values ptr keys values)
        (loop for i from 0 below length
              for key = (cffi:translate-from-foreign (cffi:mem-aref keys :pointer i) key-type)
              for value = (cffi:translate-from-foreign (cffi:mem-aref values :pointer i) value-type)
              do (setf (gethash key table) value))
        table))))
