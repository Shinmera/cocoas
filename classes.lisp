(in-package #:org.shirakumo.cocoas)

(defvar *classdefs* (make-hash-table :test 'equal))

(defmacro define-objc-class (name superclass &body methods)
  `(let ((def (or (gethash ,name *classdefs*)
                  (setf (gethash ,name *classdefs*) (list NIL (make-hash-table :test 'equal))))))
     (setf (first def) ,superclass)
     ,@(loop for (method . args) in methods
             collect `(define-objc-method (,name ,method) ,@args))
     (when *init*
       (register-class ,name (first def) (second def)))
     ,name))

(defun encode-types (types)
  (with-output-to-string (out)
    (dolist (type types)
      (case type
        ((:id objc:id) (write-char #\@ out))
        ((:sel objc:sel) (write-char #\: out))
        ((:class objc:oclass) (write-char #\# out))
        (:string (write-char #\* out))
        (:void (write-char #\v out))
        ((:bool :boolean) (write-char #\B out))
        (:double (write-char #\d out))
        (:float (write-char #\f out))
        ((:ulonglong :unsigned-long-long) (write-char #\Q out))
        ((:ulong :unsigned-long) (write-char #\L out))
        ((:ushort :unsigned-short) (write-char #\S out))
        ((:uint :unsigned-int) (write-char #\I out))
        ((:uchar :unsigned-char) (write-char #\C out))
        ((:longlong :long-long) (write-char #\q out))
        ((:long :long) (write-char #\l out))
        ((:short :short) (write-char #\s out))
        ((:int :int) (write-char #\i out))
        ((:char :char) (write-char #\c out))
        (:pointer (write-char #\? out))
        (T (etypecase type
             (cons (destructuring-bind (kind inner) type
                     (ecase kind
                       (:struct
                        (format out "[~a=~a]"
                                (cffi:translate-camelcase-name inner)
                                (encode-types (loop for name in (cffi:foreign-slot-names type)
                                                    collect (cffi:foreign-slot-type type name)))))
                       (:pointer
                        (format out "^~a" (encode-types (list inner)))))))
             (symbol type)))))))

(defun normalize-type (type)
  (case type 
    (:id 'objc:id)
    (:sel 'objc:sel)
    (:class 'objc:oclass)
    (T type)))

(defmacro define-objc-method ((class method) rettype args &body body)
  (let ((cbname (intern (format NIL "%~a-~a" (string-upcase class) method)))
        (types (encode-types (list* rettype 'objc:id 'objc:sel (mapcar #'second args))))
        (args (loop for (var type) in args
                    collect (list var (normalize-type type))))
        (self (intern (string :self))))
    `(progn 
       (let ((def (or (gethash ,class *classdefs*)
                      (error "No such class ~s" ,class))))
         (setf (gethash ,(to-method-name method) (second def)) (list ',cbname ,types)))
       
       (cffi:defcallback ,cbname ,(normalize-type rettype) ((,self :pointer) (command objc:sel) ,@args)
         (declare (ignorable ,self command))
         ,@body))))

(defun register-class (name superclass methods)
  (let ((cls (objc:class-allocate-class superclass name 0)))
    (loop for name being the hash-keys of methods using (hash-value methoddef)
          for (impl types) = methoddef
          do (objc:class-add-method cls (objc:register-name name) (cffi:get-callback impl) types))
    (objc:class-register-class cls)))

(defun register-classes ()
  (loop for name being the hash-keys of *classdefs* using (hash-value classdef)
        for (superclass methods) = classdef
        do (register-class name superclass methods)))
