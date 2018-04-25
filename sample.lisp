(asdf:oos 'asdf:load-op "cffi")
   
(defpackage #:sample
  (:use :common-lisp
	:cffi))

(in-package #:cffi)
(defmacro defcstruct (name-and-options &body fields)
  "Define the layout of a foreign structure."
  (discard-docstring fields)
  (destructuring-bind (name . options)
      (ensure-list name-and-options)
    (let ((conc-name (getf options :conc-name)))
      (remf options :conc-name)
      (unless (getf options :class) (setf (getf options :class) (symbolicate name '-tclass)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ;; m-f-s-t could do with this with mop:ensure-class.
         ,(when-let (class (getf options :class))
            `(defclass ,class (foreign-struct-type
                               translatable-foreign-type)
               ()))
         (notice-foreign-struct-definition ',name ',options ',fields)
         ,@(when conc-name
             (generate-struct-accessors name conc-name
                                        (mapcar #'car fields)))
         ,@(when *defcstruct-hook*
             ;; If non-nil, *defcstruct-hook* should be a function
             ;; of the arguments that returns NIL or a list of
             ;; forms to include in the expansion.
             (apply *defcstruct-hook* name-and-options fields))
         (define-parse-method ,name ()
           (parse-deprecated-struct-type ',name :struct))
         '(:struct ,name)))))

(in-package #:sample)

(define-foreign-library libsample
  (:unix "libsample.so" :search-path "."))

(use-foreign-library libsample)

;(defcstruct sample
(defcstruct (sample :class sample-type)
	    (f :float)
	    (i :int)
	    (s :char :count 80)
	    (p (:pointer :void)))

(defmethod translate-from-foreign (pointer (type sample-type))
  (call-next-method))
(translation-forms-for-class sample-type sample-type)

(defcfun "get_sample" (:pointer (:struct sample)))

(defcfun "free_sample" :void
  (example (:pointer (:struct sample))))
