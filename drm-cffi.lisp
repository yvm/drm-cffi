;(asdf:oos 'asdf:load-op :cffi)
;(asdf:oos 'asdf:load-op :sb-posix)
(asdf:load-system 'cffi)
(asdf:load-system 'sb-posix)

(defpackage #:libdrm-cffi
  (:use #:common-lisp
	#:cffi)
  (:nicknames #:drm))

(in-package #:sb-posix)
(unless (boundp 'o-cloexec)
  (defconstant o-cloexec #x80000)
  (export 'o-cloexec))

(in-package #:libdrm-cffi)

(define-foreign-library libdrm
  (:unix (:or "libdrm.so" "libdrm.so.2"))
  (t (:default "libdrm")))
(use-foreign-library libdrm)

;;; xf86drmMode.h #definitions
;;; should be grovelled
(defconstant +drm-display-mode-len+ 32)
(defconstant +drm-cap-dumb-buffer+ 1) ; drm.h: #define DRM_CAP_DUMB_BUFFER 0x1
(defconstant +drm-ioctl-mode-create-dumb+ 3223348402)
(defconstant +drm-ioctl-mode-map-dumb+ 3222299827)
(defconstant +drm-ioctl-mode-destroy-dumb+ 3221513396)
(defconstant +drm-prop-name-len+ 32) ; drm_mode.h: #define DRM_PROP_NAME_LEN	32

;;; from stdint.h
(defctype uint64-t :unsigned-long)
(defctype uint32-t :unsigned-int)
(defctype uint16-t :unsigned-short)
(defctype uint8-t :unsigned-char)

;; from drm.h
(defctype u64 uint64-t)
(defctype u32 uint32-t)


;; xf86drm.h
;; extern int           drmGetCap(int fd, uint64-t capability, uint64-t *value);
(defcfun (get-capabilities "drmGetCap") :int
  ""
  (fd :int)
  (capability uint64-t)
  (value* (:pointer uint64-t)))

(defun close-dri-file (dri-fd)
  (sb-posix:close dri-fd))

;; file should be #P"/dev/dri/card0"
(defun open-dri-file (file) ; returns C open() file designator, which is integer
  ""
  (let ((dri-fd (sb-posix:open file (logior sb-posix:o-rdwr
					    sb-posix:o-cloexec))))
    (when (< dri-fd 3) (error "Error opening DRM device ~s." file))
    (with-foreign-object (has-dumb-buffers 'uint64-t)
;      (setf (mem-ref has-dumb-buffers 'uint64-t) 0)
      (if (zerop (get-capabilities dri-fd
				   +drm-cap-dumb-buffer+
				   has-dumb-buffers))
	  (if (zerop (mem-ref has-dumb-buffers 'uint64-t))
	      (progn
		(sb-posix:close dri-fd)
		(error "DRM device ~s does not support dumb buffers.~%" file))
	      dri-fd)
	  (progn (sb-posix:close dri-fd)
		 (error "Can not retreive capabilities for DRM device ~s.~%"
			file))))))

(defmacro with-open-dri-file ((fd-name filespec) &body body)
  `(let ((,fd-name (open-dri-file ,filespec)))
     (unwind-protect
;	  (multiple-value-prog1
	  (progn ,@body)
       (close-dri-file ,fd-name))))


;; (defctype uint32-t* (:pointer uint32-t))

;; (defmethod translate-from-foreign (pointer (type (eql 'uint32-t*)))
;;   (mem-ref pointer 'uint32-t))


(defun concat-symbol (atom-1 atom-2)
  (intern
   (string-upcase
    (concatenate 'string
		 (princ-to-string atom-1)
		 (princ-to-string atom-2)))))

(defun concat-foreign-name (prefix suffix)
  (concatenate 'string
	       prefix
	       (if (symbolp suffix)
		   (remove #\- (write-to-string suffix :case :capitalize))
		   suffix)))

(defun convert-foreign-list (pointer count type)
  (when (and count
	     (> count 0))
    (cons (if (and (listp (second type))
		   (equal (first (second type))
			  :struct))
	      (funcall (concat-symbol "MAKE-" (second (second type)))
		       (mem-aptr pointer (second type) (- count 1)))
	      (mem-aref pointer (second type) (- count 1)))
	  (convert-foreign-list pointer (- count 1) type))))

(defun convert-foreign-slots (foreign-slots-defs)
  (when foreign-slots-defs
    (let ((count-slot
	   (find-if (lambda (slot)
		      (let ((member-name (symbol-name (first slot))))
			(string= "COUNT"
				 (subseq member-name 0 (position #\- member-name)))))
		    foreign-slots-defs)))
      (if count-slot
	  (let* ((name-root (string-upcase
			     (subseq (symbol-name (first count-slot))
				     (+ (position #\- (symbol-name (first count-slot)))
					1)
				     (- (length (symbol-name (first count-slot)))
					1))))
		 (ids-slot
		  (find (intern (string-upcase (concatenate 'string name-root "S")))
			foreign-slots-defs :key #'car))
		 (values-slot
		  (find (intern (string-upcase (concatenate 'string name-root "-VALUES")))
			foreign-slots-defs :key #'car)))
;	    (format t "~%~A~%~A~%~A~%" name-root ids-slot values-slot)
	    (convert-foreign-slots
	     (append (when ids-slot
		       (list (list (first ids-slot)
				   :type 'cons
				   ;; :documentation (write-to-string (second (second ids-slot)))
				   )))
		     (when values-slot
		       (list (list (first values-slot)
				   :type 'cons)))
		     (remove-if (lambda (slot)
				  (member slot (list count-slot
						     ids-slot
						     values-slot)))
				foreign-slots-defs))))
	  (mapcar (lambda (slot)
		    (append (if (equal (member :type slot)
				       '(:type cons)) ; FIXIT when append more elements on count-slot case
				slot
				(list (first slot)))
			  (list :initarg (intern (symbol-name (first slot)) "KEYWORD"))))
		  foreign-slots-defs)))))

(defun initargs-for-make-instance (foreign-slots converted-slots params-with-values)
  (when converted-slots
    (append (list (second (member :initarg (first converted-slots))))
	    (list (let* ((param-name (first (first converted-slots)))
			 (param-value (getf params-with-values param-name)))
		    (if (pointerp param-value)
			(if (eq (second (member :type (first converted-slots)))
				'cons)
			    (let* ((param-name-str (symbol-name param-name))
				   (count-value
				    (getf params-with-values
					  (concat-symbol
					   "COUNT-"
					   (if (and (> (length param-name-str)
						       7)
						    (string= "-VALUES"
							     (subseq param-name-str
								     (- (length param-name-str)
									7))))
					       (concatenate 'string
							    (subseq param-name-str
								    0
								    (- (length param-name-str)
								       7))
							    "S")
					       param-name-str))))
				   (type (second (find param-name
						       foreign-slots
						       :key #'car))))
					;			  (format t "~%~%count: ~A~%type: ~A~%~%" count type)
			      (convert-foreign-list param-value count-value type))
			    (foreign-string-to-lisp param-value))
			param-value)))
		  (initargs-for-make-instance foreign-slots
					      (rest converted-slots)
					      params-with-values))))

(defmacro c-struct-translator (name slots
			       &key (get-func nil get-func-set-p)
				 (free-func nil free-func-set-p)
				 without-id-param)
  (let ((name-type (concat-symbol name "-type"))
	(name* (concat-symbol name "*"))
	(get-name (concat-symbol "get-" name))
	(free-name (concat-symbol "free-" name))
	(make-name (concat-symbol "make-" name))
	(drm-mode-get-name (unless (and get-func-set-p
					(not get-func))
			     (if get-func
				 get-func
				 (concat-foreign-name "drmModeGet" name))))
	(drm-mode-free-name (unless (and free-func-set-p
					 (not free-func))
			      (if free-func
				  free-func
				  (concat-foreign-name "drmModeFree" name))))
	(name-id (concat-symbol name "-id"))
	(converted-slots (convert-foreign-slots slots)))
    `(progn
       (defcstruct (,name :class ,name-type)
	 ,@slots)
       (defctype ,name* (:pointer (:struct ,name)))
       (defmethod translate-from-foreign (value (type ,name-type))
	 (initargs-for-make-instance ',slots
				     ',converted-slots
                                     (call-next-method)))
       ,@(when drm-mode-get-name
	       (list `(defcfun (,get-name ,drm-mode-get-name) ,name*
			(fd :int)
			,@(unless without-id-param
				  (list (list name-id 'uint32-t))))))
       ,@(when drm-mode-free-name
	       (list `(defcfun (,free-name ,drm-mode-free-name) :void
			(ptr ,name*))))
       (defclass ,name ()
	 ,converted-slots)
       (defmethod print-object ((object ,name) stream)
      	 (print-unreadable-object (object stream :type t)
       	   (dolist (slot ',converted-slots)
	     (format stream "~A: ~A~%" (first slot) (slot-value object (first slot))))))
;	   (format stream "~A" (mapcar #'sb-mop:slot-definition-name (sb-mop:class-slots (class-of object))))))
       (defun ,make-name ,(if drm-mode-get-name
			      (cons 'fd (unless without-id-param
					  (list name-id)))
			      (list 'foreign-pointer))
	 (with-foreign-object (struct* ',name*)
	   (setf struct* ,(if drm-mode-get-name
			      (append (list get-name 'fd)
				      (unless without-id-param
					(list name-id)))
			      'foreign-pointer))
	   (if (null-pointer-p struct*)
	       (format t "Cannot get ~A~%" ',name)
	       (multiple-value-prog1
		   (apply #'make-instance (cons ',name (mem-ref struct* '(:struct ,name))))
		 ,@(when drm-mode-free-name
			   (list `(,free-name struct*))))))))))

(c-struct-translator fb
		     ((fb-id uint32-t)
		      (width uint32-t)
		      (height uint32-t)
		      (pitch uint32-t)
		      (bpp uint32-t)
		      (depth uint32-t)
		      (handle uint32-t))
		     :get-func "drmModeGetFB"
		     :free-func "drmModeFreeFB")

(defcfun (add-fb "drmModeAddFB") :int
  (fd :int)
  (width uint32-t)
  (height uint32-t)
  (depth uint8-t)
  (bpp uint8-t)
  (pitch uint32-t)
  (bo-handle uint32-t)
  (buf-id* (:pointer uint32-t)))

(defcfun (add-fb2 "drmModeAddFB2") :int
  (fd :int)
  (width uint32-t)
  (height uint32-t)
  (pixel-format uint32-t)
  (bo-handles (:pointer uint32-t)) ; :count 4
  (pitches (:pointer uint32-t)) ; :count 4
  (offsets (:pointer uint32-t)) ; :count 4
  (buf-id (:pointer uint32-t))
  (flags uint32-t))

(defcfun (remove-fb "drmModeRmFB") :int
  (fd :int)
  (buffer-id uint32-t))

;; drm.h:85
(defcstruct clip-rect
  (x1 :unsigned-short)
  (y1 :unsigned-short)
  (x2 :unsigned-short)
  (y2 :unsigned-short))

(defcfun (dirty-fb "drmModeDirtyFB") :int
  (fd :int)
  (buffer-id uint32-t)
  (clips (:pointer (:struct clip-rect)))
  (num-clips uint32-t))




(defcenum connection
  ""
  (:drm-mode-connected 1)
  :drm-mode-disconnected
  :drm-mode-unknownconnection)

(defcenum sub-pixel
  ""
  (:drm-mode-subpixel-unknown 1)
  :drm-mode-subpixel-horizontal-rgb
  :drm-mode-subpixel-horizontal-bgr
  :drm-mode-subpixel-vertical-rgb
  :drm-mode-subpixel-vertical-bgr
  :drm-mode-subpixel-none)

(c-struct-translator mode-info
		     ((clock uint32-t)
		      (hdisplay uint16-t)
		      (hsync-start uint16-t)
		      (hsync-end uint16-t)
		      (htotal uint16-t)
		      (hskew uint16-t)
		      (vdisplay uint16-t)
		      (vsync-start uint16-t)
		      (vsync-end uint16-t)
		      (vtotal uint16-t)
		      (vscan uint16-t)
		      (vrefresh uint32-t)
		      (flags uint32-t)
		      (type uint32-t)
		      (name :char :count #.+drm-display-mode-len+))
		     :get-func nil
		     :free-func nil)

(c-struct-translator encoder
		     ((encoder-id uint32-t)
		      (encoder-type uint32-t)
		      (crtc-id uint32-t)
		      (possible-crtcs uint32-t)
		      (possible-clones uint32-t)))

(c-struct-translator connector
		     ((connector-id uint32-t)
		      (encoder-id uint32-t)
		      (connector-type uint32-t)
		      (connector-type-id uint32-t)
		      (connection connection)
		      (mm-width uint32-t)
		      (mm-height uint32-t)
		      (subpixel sub-pixel)
		      (count-modes :int)
		      (modes (:pointer (:struct mode-info)))
		      (count-props :int)
		      (props (:pointer uint32-t))
		      (prop-values (:pointer uint64-t))
		      (count-encoders :int)
		      (encoders (:pointer uint32-t))))

(c-struct-translator crtc
		     ((crtc-id uint32-t)
		      (buffer-id uint32-t)
		      (x uint32-t)
		      (y uint32-t)
		      (width uint32-t)
		      (height uint32-t)
		      (mode-valid :int)
		      (mode (:struct mode-info))
		      (gamma-size :int)))

(defcfun (set-crtc "drmModeSetCrtc") :int
  "Set the mode on a crtc crtcId with the given mode modeId."
  (fd :int)
  (crtc-id uint32-t)
  (buffer-id uint32-t)
  (x uint32-t)
  (y uint32-t)
  (connectors* (:pointer uint32-t))
  (count :int)
  (mode* mode-info*))

(c-struct-translator resources
		     ((count-fbs :int)
		      (fbs (:pointer uint32-t))
		      (count-crtcs :int)
		      (crtcs (:pointer uint32-t))
		      (count-connectors :int)
		      (connectors (:pointer uint32-t))
		      (count-encoders :int)
		      (encoders (:pointer uint32-t))
		      (min-width uint32-t)
		      (max-width uint32-t)
		      (min-height uint32-t)
		      (max-height uint32-t))
		     :without-id-param t)



(defmethod translate-from-foreign (value (type (eql mode-info-type)))
  (let ((cnm (call-next-method)))
	     (format t "call-next-method values: ~A" cnm))
	   (translation-forms-for-class value ,name-type))






;; xf86drmMode.h
;; typedef struct _drmModePropertyBlob {
;; 	uint32_t id;
;; 	uint32_t length;
;; 	void *data;
;; } drmModePropertyBlobRes, *drmModePropertyBlobPtr;
(defcstruct property-blob :class property-blob-type
  (id uint32-t)
  (length uint32-t)
  (data* (:pointer :void)))
(defctype property-blob* (:pointer (:struct property-blob)))

;; libdrm/drm_mode.h
;; struct drm_mode_property_enum {
;; 	__u64 value;
;; 	char name[DRM_PROP_NAME_LEN];
;; };
(defcstruct property-enum :class property-enum-type
  (value u64)
  (name :char :count #.+drm-prop-name-len+))

;; xf86drmMode.h
;; typedef struct _drmModeProperty {
;; 	uint32_t prop_id;
;; 	uint32_t flags;
;; 	char name[DRM_PROP_NAME_LEN];
;; 	int count_values;
;; 	uint64_t *values; /* store the blob lengths */
;; 	int count_enums;
;; 	struct drm_mode_property_enum *enums;
;; 	int count_blobs;
;; 	uint32_t *blob_ids; /* store the blob IDs */
;; } drmModePropertyRes, *drmModePropertyPtr;
(defcstruct property :class property-type
  (prop-id uint32-t)
  (flags uint32-t)
  (name :char :count #.+drm-prop-name-len+)
  (count-values :int)
  (values* (:pointer uint64-t))
  (count-enums :int)
  (enums* (:pointer (:struct property-enum)))
  (count-blobs :int)
  (blob-ids* uint32-t))
(defctype property* (:pointer (:struct property)))





;; xf86drmMode.h
;; typedef enum {
;; 	DRM_MODE_CONNECTED         = 1,
;; 	DRM_MODE_DISCONNECTED      = 2,
;; 	DRM_MODE_UNKNOWNCONNECTION = 3
;; } drmModeConnection;
(defcenum connection
  ""
  (:drm-mode-connected 1)
  :drm-mode-disconnected 2
  :drm-mode-unknownconnection 3)


;; xf86drmMode.h
;; typedef enum {
;; 	DRM_MODE_SUBPIXEL_UNKNOWN        = 1,
;; 	DRM_MODE_SUBPIXEL_HORIZONTAL_RGB = 2,
;; 	DRM_MODE_SUBPIXEL_HORIZONTAL_BGR = 3,
;; 	DRM_MODE_SUBPIXEL_VERTICAL_RGB   = 4,
;; 	DRM_MODE_SUBPIXEL_VERTICAL_BGR   = 5,
;; 	DRM_MODE_SUBPIXEL_NONE           = 6
;; } drmModeSubPixel;
(defcenum sub-pixel
  ""
  (:drm-mode-subpixel-unknown 1)
  :drm-mode-subpixel-horizontal-rgb 2
  :drm-mode-subpixel-horizontal-bgr 3
  :drm-mode-subpixel-vertical-rgb 4
  :drm-mode-subpixel-vertical-bgr 5
  :drm-mode-subpixel-none 6)

;; xf86drmMode.h
;; typedef struct _drmModeConnector {
;; 	uint32_t connector_id;
;; 	uint32_t encoder_id; /**< Encoder currently connected to */
;; 	uint32_t connector_type;
;; 	uint32_t connector_type_id;
;; 	drmModeConnection connection;
;; 	uint32_t mmWidth, mmHeight; /**< HxW in millimeters */
;; 	drmModeSubPixel subpixel;
;; 	int count_modes;
;; 	drmModeModeInfoPtr modes;
;; 	int count_props;
;; 	uint32_t *props; /**< List of property ids */
;; 	uint64_t *prop_values; /**< List of property values */
;; 	int count_encoders;
;; 	uint32_t *encoders; /**< List of encoder ids */
;; } drmModeConnector, *drmModeConnectorPtr;
(defcstruct connector
  (connector-id uint32-t)
  (encoder-id uint32-t)
  (connector-type uint32-t)
  (connector-type-id uint32-t)
  (connection connection)
  (mm-width uint32-t)
  (mm-height uint32-t)
  (subpixel sub-pixel)
  (count-modes :int)
  (modes* mode-info*)
  (count-props :int)
  (props* (:pointer uint32-t))
  (prop-values* (:pointer uint64-t))
  (count-encoders :int)
  (encoders* (:pointer uint32-t)))
(defctype connector* (:pointer (:struct connector)))

;; extern drmModeConnectorPtr drmModeGetConnector(int fd,
;;					          uint32-t connectorId);
(defcfun (get-connector "drmModeGetConnector") connector*
  "Retrieve all information about the connector connectorId. This will do a
forced probe on the connector to retrieve remote information such as EDIDs
from the display device."
  (fd :int)
  (connector_id uint32-t))

;; extern void drmModeFreeConnector( drmModeConnectorPtr ptr );
(defcfun (free-connector "drmModeFreeConnector") :void
  ""
  (ptr connector*))


;; xf86drmMode.h
;; typedef struct _drmModeObjectProperties {
;; 	uint32_t count_props;
;; 	uint32_t *props;
;; 	uint64_t *prop_values;
;; } drmModeObjectProperties, *drmModeObjectPropertiesPtr;
(defcstruct object-properties
  (count-props uint32-t)
  (props* (:pointer uint32-t))
  (prop_values* (:pointer uint64-t)))
(defctype object-properties* (:pointer (:struct object-properties)))

;; xf86drmMode.h
;; typedef struct _drmModePlane {
;; 	uint32_t count_formats;
;; 	uint32_t *formats;
;; 	uint32_t plane_id;
;; 	uint32_t crtc_id;
;; 	uint32_t fb_id;
;; 	uint32_t crtc_x, crtc_y;
;; 	uint32_t x, y;
;; 	uint32_t possible_crtcs;
;; 	uint32_t gamma_size;
;; } drmModePlane, *drmModePlanePtr;
(defcstruct plane
  (count-formats uint32-t)
  (formats* (:pointer uint32-t))
  (plane-id uint32-t)
  (crtc-id uint32-t)
  (fb-id uint32-t)
  (crtc-x uint32-t)
  (crtc-y uint32-t)
  (x uint32-t)
  (y uint32-t)
  (possible-crtcs uint32-t)
  (gamma-size uint32-t))
(defctype plane* (:pointer (:struct plane)))

;; xf86drmMode.h
;; typedef struct _drmModePlaneRes {
;; 	uint32_t count_planes;
;; 	uint32_t *planes;
;; } drmModePlaneRes, *drmModePlaneResPtr;
(defcstruct plane-resources
  (count-planes uint32-t)
  (planes* (:pointer uint32-t)))
(defctype plane-resources* (:pointer (:struct plane-resources)))

;; xf86drm.h
;; typedef struct _drmEventContext
(defcstruct event-context
  ""
  ;; This struct is versioned so we can add more pointers if we
  ;; add more events.
  (version :int)
  (vblank-handler* (:pointer :void))
  (page-flip-handler* (:pointer :void)))
;; (defctype event-context* (:pointer (:struct event-context)))

;; libdrm/drm_mode.h
;; struct drm_mode_create_dumb
(defcstruct create-dumb
  ""
  (height u32)
  (width u32)
  (bpp u32)
  (flags u32)
  (handle u32)
  (pitch u32)
  (size u64))

(defcstruct map-dumb
  "libdrm/drm_mode.h
struct drm_mode_map_dumb"
  (handle u32)
  (pad u32)
  ;; Fake offset to use for subsequent mmap call
  ;; This is a fixed-size type for 32/64 compatibility.
  (offset u64))

(defcstruct destroy-dumb
  "libdrm/drm_mode.h
struct drm_mode_destroy_dumb "
  (handle u32))


;;; Functions





;; TODO: check if all needed drm*Free* functions defined and used when should

;; libkms/libkms.h
;; int kms_create(int fd, struct kms_driver **out);
(defcfun (drm-ioctl "drmIoctl") :int
  ""
  (fd :int)
  (request :unsigned-long)
  (arg* (:pointer :void)))


;; xf86drmMode.h
;; extern int drmModePageFlip(int fd, uint32-t crtc_id, uint32-t fb_id,
;;			   uint32-t flags, void *user_data);
(defcfun (page-flip "drmModePageFlip") :int
  (fd :int)
  (crtc-id uint32-t)
  (fb-id uint32-t)
  (flags uint32-t)
  (user-data* (:pointer :void)))

;; ;; Native classes
(defstruct buffer-object
  ;; width
  ;; height
  ;; depth
  ;; bits-per-pixel
  pitch
  size
  handle
  fb-id
  map-sap)

(defstruct mode
  (name "" :type string)
  hdisplay
  vdisplay)

(defstruct connector
  id
  status
  crtc-id
  saved-crtc	 ; foreign crtc struct saved to restore previous state
  (modes '())
  (encoders '()))	; list of encoders supported by this connector

(defstruct encoder
  id
  type
  crtc-ids		   ; list of IDs of CRTCs supported by encoder
  clones)

;; (defstruct crtc
;;   id
;;   buffer-id
;;   x
;;   y
;;   mode)

(defstruct device
  fd
  (connectors '())
  (crtcs-count '())
  (framebuffers '()))


(defun reset-foreign-struct (object type)
  (dolist (slot (foreign-slot-names type))
    (setf (foreign-slot-value object type slot) 0)))

(defun destroy-dumb-buffer (dri-fd handle)
  (with-foreign-object (destroy-request
			'(:struct drm_mode_destroy_dumb))
    (reset-foreign-struct destroy-request
			  '(:struct drm_mode_destroy_dumb))
    (setf (foreign-slot-value destroy-request
			      '(:struct drm_mode_destroy_dumb)
			      'handle)
	  handle)
    (drmIoctl dri-fd
	      +drm-ioctl-mode-destroy-dumb+
	      destroy-request)))

(defun create-dumb-buffer (dri-fd width height bits-per-pixel)
  (with-foreign-object (create-request '(:struct drm_mode_create_dumb))
    (reset-foreign-struct create-request '(:struct drm_mode_create_dumb))
    (setf (foreign-slot-value create-request
			      '(:struct drm_mode_create_dumb)
			      'width)
	  width)
    (setf (foreign-slot-value create-request
			      '(:struct drm_mode_create_dumb)
			      'height)
	  height)
    (setf (foreign-slot-value create-request
			      '(:struct drm_mode_create_dumb)
			      'bpp)
	  bits-per-pixel)
    (if (< (drmIoctl dri-fd
		     +drm-ioctl-mode-create-dumb+
		     create-request)
	   0)
	(format t "Cannot create dumb buffer.~%")
	(values (foreign-slot-value create-request
				    '(:struct drm_mode_create_dumb)
				    'pitch)
		(foreign-slot-value create-request
				    '(:struct drm_mode_create_dumb)
				    'size)
		(foreign-slot-value create-request
				    '(:struct drm_mode_create_dumb)
				    'handle)))))

(defun get-frame-buffer-id (dri-fd width height depth
			    bits-per-pixel pitch handle)
  (with-foreign-object (frame-buffer-id 'uint32-t)
    (if (= (drmModeAddFB dri-fd width height depth
			 bits-per-pixel pitch handle frame-buffer-id)
	   0)
	(mem-ref frame-buffer-id 'uint32-t)
	(progn (destroy-dumb-buffer dri-fd handle)
	       (format t "Cannot create frame buffer.~%")
	       nil))))

(defun get-dumb-buffer-offset (dri-fd frame-buffer-id handle)
  (with-foreign-object (map-request '(:struct drm_mode_map_dumb))
    (reset-foreign-struct map-request '(:struct drm_mode_map_dumb))
    (setf (foreign-slot-value map-request
			      '(:struct drm_mode_map_dumb)
			      'handle)
	  handle)
    (if (= (drmIoctl dri-fd
		     +drm-ioctl-mode-map-dumb+
		     map-request)
	   0)
	(foreign-slot-value map-request
			    '(:struct drm_mode_map_dumb)
			    'offset)
	(progn (drmModeRmFB dri-fd frame-buffer-id)
	       (destroy-dumb-buffer dri-fd handle)
	       (format t "Cannot get offset for dumb buffer.~%")
	       nil))))

(defun map-dumb-buffer (dri-fd frame-buffer-id handle size offset)
  (let ((map-sap (sb-posix:mmap nil size
				(logior sb-posix:prot-read
					sb-posix:prot-write)
				sb-posix:map-shared
				dri-fd
				offset)))
    ; FIXIF: perform somehow error handling (condition?)
   ;; (if (sb-posix::sap-or-nil map-sap)
    ;;   (progn (drmModeRmFB dri-fd frame-buffer-id)
    ;; 	     (destroy-dumb-buffer dri-fd handle)
    ;; 	     (format t "Cannot map dumb buffer.~%")
    ;; 	     nil)
    ;;   (progn
    (do ((offset 0 (+ offset 4))) ; FIXIT: different step/sap-ref- for different depth/bpp
	((> offset size))
	(setf (sb-sys:sap-ref-64 map-sap offset) 0))
    map-sap))

(defun destroy-buffer-object (dri-fd buffer-object)
  (sb-posix:munmap (buffer-object-map-sap buffer-object)
		   (buffer-object-size buffer-object))
  (drmModeRmFB dri-fd (buffer-object-fb-id buffer-object))
  (destroy-dumb-buffer dri-fd (buffer-object-handle buffer-object)))

(defun create-buffer-object (dri-fd width height depth
			     bits-per-pixel)
  (multiple-value-bind (pitch size handle)
      (create-dumb-buffer dri-fd width height bits-per-pixel)
    (when (and pitch size handle)
      (format t "pitch=~d~%size=~d~%handle=~d~%" pitch size handle)
      (let ((frame-buffer-id (get-frame-buffer-id dri-fd
						  width height depth
						  bits-per-pixel
						  pitch handle)))
	(when frame-buffer-id
	  (format t "frame-buffer-id=~d~%" frame-buffer-id)
	  (let ((offset (get-dumb-buffer-offset dri-fd
						frame-buffer-id
						handle)))
	    (when offset
	      (format t "offset=~d~%" offset)
	      (let ((map-sap (map-dumb-buffer dri-fd
					      frame-buffer-id handle
					      size offset)))
		(when map-sap
		  (make-buffer-object :pitch pitch
				      :size size
				      :handle handle
				      :fb-id frame-buffer-id
				      :map-sap map-sap))))))))))

;(defun clear-buffer (buffer-object))

;; (defun get-crtc (dri-fd connector)
;;   )

(defun get-modes (foreign-connector)
  (let ((modes-count (foreign-slot-value foreign-connector
					 '(:struct drmModeConnector)
					 'count-modes)))
    (if (< modes-count 1)
	(format t "No modes found for connector.~%")
	(let ((modes '()))
	  (format t "Count of modes: ~d.~%" modes-count)
	  (flet ((mode-slot-value (mode-index mode-slot)
		   (foreign-slot-value
		    (mem-aptr (foreign-slot-value foreign-connector
						  '(:struct drmModeConnector)
						  'modes)
			      '(:struct drmModeModeInfo)
			      mode-index)
		    '(:struct drmModeModeInfo)
		    mode-slot)))
	    (dotimes (i modes-count)
	      (push (make-mode :name (foreign-string-to-lisp
				      (mode-slot-value i 'name))
			       :hdisplay (mode-slot-value i 'hdisplay)
			       :vdisplay (mode-slot-value i 'vdisplay))
		    modes)))
	  modes))))

(defun get-encoders (dri-fd foreign-connector)
  (let ((encoders-count (foreign-slot-value foreign-connector
					    '(:struct drmModeConnector)
					    'count-encoders)))
    (if (< encoders-count 1)
	(format t "No encoders found for connector.~%")
	(let ((encoders '()))
	  (format t "encoders-count=~d~%" encoders-count)
	  (with-foreign-object (foreign-encoder
				'(:pointer (:struct drmModeEncoder)))
	    (dotimes (i encoders-count)
	      (setf foreign-encoder ; FIXIT: -id?
		    (drmModeGetEncoder dri-fd
				       (mem-aref (foreign-slot-value
						  foreign-connector
						  '(:struct drmModeConnector)
						  'encoders)
						 'uint32-t
						 i)))
	      (if (null-pointer-p foreign-encoder)
		  (format t "Can not get encoder ~d.~%" i)
		  (flet ((encoder-slot-list (slot)
			   (let ((int-value
				  (foreign-slot-value foreign-encoder
						      '(:struct drmModeEncoder)
						      slot))
				 (list-value '()))
			     (dotimes (bit-pos 32)
			       (unless (= (ldb (byte 1 bit-pos) int-value)
					     0)
				 (push bit-pos list-value)))
			     list-value))
			 (encoder-slot-value (slot)
			   (foreign-slot-value foreign-encoder
					       '(:struct drmModeEncoder)
					       slot)))
		    (let ((encoder
			   (make-encoder
			    :id (encoder-slot-value 'encoder-id)
			    :type (encoder-slot-value 'encoder-type)
			    :crtc-id (encoder-slot-value 'crtc-id)
			    :crtcs (encoder-slot-list 'possible-crtcs)
			    :clones (encoder-slot-list 'possible-clones))))
		      (format t "encoder=~A~%" encoder)
		      (push encoder encoders))))
	      (drmModeFreeEncoder foreign-encoder)))
	  encoders))))

; why?
;; (defmacro update-connector (dri-fd connector &key encoder-id)
;;   `(with-foreign-object (foreign-connector
;; 			 '(:pointer (:struct drmModeConnector)))
;;      (setf foreign-connector
;; 	   (drmModeGetConnector ,dri-fd
;; 				(connector-id ,connector)))
;;      (let* ((connection-status
;; 	     (if (null-pointer-p foreign-connector)
;; 		 :drm-mode-unknownconnection
;; 		 (foreign-slot-value foreign-connector
;; 				     '(:struct drmModeConnector)
;; 				     'connection)))
;; 	    ,@(when (eq connection-status
;; 			:drm-mode-connected)
;; 		    (list
;; 		     `(encoder-id ,(if encoder-id
;; 				       encoder-id
;; 				       (foreign-slot-value foreign-connector
;; 							   '(:struct drmModeConnector)
;; 							   'encoder-id)))
;; 		     `(modes-list ,(get-modes dri-fd
;; 					      foreign-connector))
;; 		     `(encoders-list ,(get-encoders dri-fd
;; 						    foreign-connector)))))
;; 	 (drmModeFreeConnector foreign-connector)
;; 	 (make-connector :id (connector-id connector)
;; 			 :connection connection-status
;; 			 ,@(when (eq connection-status
;; 				     :drm-mode-connected)
;; 				 `(:encoder-id ,encoder-id
;; 					       :modes ,modes-list
;; 					       :encoders ,encoders-list))))))

;; by default use current encoder+crtc if there is one

(defun get-current-crtc-id (dri-fd foreign-connector)
  (let ((encoder-id (foreign-slot-value foreign-connector
					'(:struct drmModeConnector)
					'encoder-id)))
    (when (not (= encoder-id 0))
      (with-foreign-object (foreign-encoder '(:pointer (:struct drmModeEncoder)))
	(setf foreign-encoder (drmModeGetEncoder dri-fd encoder-id))
	(unless (null-pointer-p foreign-encoder)
	  (let ((current-crtc-id (foreign-slot-value foreign-encoder
						     '(:struct drmModeEncoder)
						     'crtc-id)))
	    (drmModeFreeEncoder foreign-encoder)
	    (unless (= current-crtc-id 0)
	      current-crtc-id)))))))

(defmacro update-connector (dri-fd connector &key (status nil)
					       (crtc-id nil)
					       (saved-crtc)
					       (encoders)))

(defun update-connector (dri-fd connector &key (crtc-id nil crtc-id-set-p))
  (with-foreign-object (foreign-connector
			'(:pointer (:struct drmModeConnector)))
    (setf foreign-connector
	  (drmModeGetConnector dri-fd (connector-id connector)))
    (let ((connection-status
	   (if (null-pointer-p foreign-connector)
	       :drm-mode-unknownconnection
	       (foreign-slot-value foreign-connector
				   '(:struct drmModeConnector)
				   'connection))))
      (if (eq connection-status
	      :drm-mode-connected)
	  (let* (;; (encoder-id (if encoder-id ; FIXIT: choose encoder-id from supported encoders list
		;; 		encoder-id ; if (eq encoder-id t)
		;; 		(foreign-slot-value foreign-connector
		;; 				    '(:struct drmModeConnector)
		;; 				    'encoder-id)))
		 (saved-crtc-id (unless (connector-saved-crtc connector)
				  (get-current-crtc-id dri-fd foreign-connector))
		 (crtc-id (if saved-crtc-id
			      ))
;		(modes (get-modes foreign-connector))
		(encoders (get-encoders dri-fd foreign-connector)))
	    (let ((saved-crtc (if (and (not encoder-id-set-p)
				       (not (= encoder-id 0))
				       (not (connector-saved-crtc connector))) ; initialization
				  (drmModeGetCrtc dri-fd ))))
	    (drmModeFreeConnector foreign-connector)
	    (make-connector :id (connector-id connector)
			    :status connection-status
			    :crtc-id
			    :saved-crtc (drmModeGetCrtc dri-fd saved-crtc-id)
;			    :modes modes
			    :encoders encoders))
	  (progn
	    (drmModeFreeConnector foreign-connector)
	    (make-connector :id (connector-id connector)
			    :status connection-status))))))


;(defun init-connector (dri-fd connector-id)

(defun init-connectors (dri-fd resources)
  ""
  (let ((connectors-count (foreign-slot-value resources
					      '(:struct drmModeRes)
					      'count-connectors)))
    (if (< connectors-count 1)
      (format t "No connectors found.~%")
      (let ((connectors '()))
	(format t "Connectors count: ~d.~%" connectors-count)
	(dotimes (i connectors-count)
	  (let ((connector-id
		 (mem-aref (foreign-slot-value resources
					       '(:struct drmModeRes)
					       'connectors)
			   'uint32-t
			   i)))
	    (with-foreign-object (connector connector*)
	      (setf connector (get-connector dri-fd connector-id))
	      (if (null-pointer-p connector)
		  (format t "Cannot get connector ~d." connector-id)
		  (progn
		    (push connector connectors)
		    (free-connector connector))))))
	connectors))))

(defun init-device (dri-fd)
  ""
  (with-foreign-object (resources resources*)))
    (setf resources (get-resources dri-fd))
    (if (null-pointer-p resources)
	(format t "Can not get DRM resources.~%")
	(let ((connectors (init-connectors dri-fd resources)))
	  (drmModeFreeResources resources)
	  (if connectors
	      ;; FIXIT: take parameters from connector
	      (let ((buffer (create-buffer-object dri-fd 1920 1080 24 32)))
		(when buffer
		  (destroy-buffer-object dri-fd buffer))
					;            (make-device :crtcs crtcs)
		(format t "Device not created.~%"))
	      )
	  (make-device :connectors connectors
		       :crtcs-count crtcs-count)))))


(with-open-dri-file (fd #P"/dev/dri/card0")
       (let* ((res (make-resources fd))
	      (con (make-connector fd (second (slot-value res 'connectors)))))
	 (format t "~A" con)))
