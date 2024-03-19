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

(defun shutdown ())

(defun translate-method-name (name)
  (etypecase name
    (string
     name)
    (symbol
     (with-output-to-string (out)
       (loop with upcase = NIL
             for char across (string name)
             do (case char
                  (#\/ (write-char #\: out))
                  (#\- (setf upcase T))
                  (T (if upcase
                         (write-char (char-upcase char) out)
                         (write-char (char-downcase char) out))
                   (setf upcase NIL))))))))

(defmacro define-objcfun (class name rettype &body args)
  (destructuring-bind (name &optional method) (if (listp name) name (list name))
    (unless method
      (setf method (translate-method-name name)))
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
      (setf method (translate-method-name name)))
    (let ((self (gensym "SELF")))
      `(defun ,name (,self ,@(loop for (name) in args collect name))
         (objc:call ,self ,method
                    ,@(loop for (name type) in args
                            collect type
                            collect name)
                    ,rettype)))))

(defmacro with-objects (bindings &body body)
  (if bindings
      (destructuring-bind (var init &optional fail) (pop bindings)
        `(let ((,var ,init))
           (if (cffi:null-pointer-p ,var)
               ,(or fail `(error "The ObjC call to ~a failed." ',(car init)))
               (unwind-protect
                    (progn ,@body)
                 (objc:free ,var)))))
      `(progn ,@body)))

(defmacro with-foundation-objects (bindings &body body)
  (if bindings
      (destructuring-bind (var init &optional fail) (pop bindings)
        `(let ((,var ,init))
           (if (cffi:null-pointer-p ,var)
               ,(or fail `(error "The OS call to ~a failed." ',(car init)))
               (unwind-protect
                    (progn ,@body)
                 (objc:release ,var)))))
      `(progn ,@body)))

(defun process-event (&optional (app objc:app))
  (let ((event (objc:call app "nextEventMatchingMask:untilDate:inMode:dequeue:"
                          objc:event-mask :any
                          :pointer (objc:call "NSDate" "distantPast")
                          :pointer objc:default-run-loop-mode
                          :bool T)))
    (unless (cffi:null-pointer-p event)
      (objc:call app "sendEvent:" :pointer event))))
