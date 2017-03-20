(in-package #:clim-sdl2)

(setf (get :sdl2 :port-type) 'sdl2-port)
(setf (get :sdl2 :server-path-parser) #'(lambda (server-path)
                                          (declare (ignore server-path))
                                          (list :sdl2)))

(defclass sdl2-port (frame-manager standard-port standard-pointer)
  ())

;;                   #+unix (directory #p"/usr/share/fonts/**/*mono.*tf")
;;                   #-unix (error "Initform for font-pathnames not implemented")

(defmethod initialize-instance :after ((instance sdl2-port)
                                       &key &allow-other-keys)
  (with-slots (port frame-managers)
      instance
    (setf port instance)
    (push instance frame-managers))
  (sdl2:init :video)

  ;;(make-cursor-table port)    
  ;;(make-graft port)
  ;; (when clim-sys:*multiprocessing-p*
  ;;   (setf
  ;;    (port-event-process instance)
  ;;    (clim-sys:make-process #'(lambda ()
  ;;                                (sdl2:with-event-loop
  ;;                               (with-simple-restart
  ;;                                   (restart-event-loop
  ;;                                    "Restart CLIM's event loop.")
  ;;                                 (loop
  ;;                                    (process-next-event instance))))
  ;;                           :name (format nil "~S's event process." instance)))))
  )

;; (defmethod get-next-event ((port sdl2-port) &key wait-function (timeout nil))
;;   (declare (ignore wait-function))
;;   (let* ((*clx-port* port)
;;          (display (clx-port-display port)))
;;     (unless (xlib:event-listen display)
;;       (xlib:display-force-output (clx-port-display port)))
;;     ; temporary solution
;;     (or (xlib:process-event
;;          (clx-port-display port) :timeout timeout :handler #'event-handler :discard-p t)
;;         :timeout)))
;;   #:invoke-with-special-choices

;;   #:make-graft

;;(defclass sdl2-graft (standard-graft) ())

;; (defmethod make-graft ((port sdl2-port)
;;                        &key (orientation :default) (units :device))
;;   (let ((graft (make-instance 'sdl2-graft
;; 		 :port port :mirror (clx-port-window port)
;; 		 :orientation orientation :units units)))
;;     (setf (sheet-region graft) (make-bounding-rectangle 0 0 (xlib:screen-width (clx-port-screen port)) (xlib:screen-height (clx-port-screen port))))
;;     (push graft (port-grafts port))
;; graft))


;;    #:medium-draw-circle*
;;    #:medium-draw-glyph
;;    #:mirror-transformation
;;    #:port-allocate-pixmap
;;    #:port-deallocate-pixmap

(defmethod port-enable-sheet ((port sdl2-port) (mirror mirrored-sheet-mixin))
  (sdl2:show-window (sheet-direct-mirror mirror)))

(defmethod port-disable-sheet ((port sdl2-port) (mirror mirrored-sheet-mixin))
  (sdl2:hide-window (sheet-direct-mirror mirror)))

;;    #:port-force-output
;;    #:port-frame-keyboard-input-focus
;;    #:port-grab-pointer

;;    #:port-mirror-height
;;    #:port-mirror-width
;;    #:port-motion-hints
;;    #:port-set-mirror-region
;;    #:port-set-mirror-transformation
;;    #:port-set-sheet-region
;;    #:port-set-sheet-transformation
;;    #:port-ungrab-pointer
;;    #:queue-callback
;;    #:set-sheet-pointer-cursor
;;    #:synthesize-pointer-motion-event
;;    #:text-style-character-width
;;    ;; From CLIM (mentioned in the spec)
;;    #:adopt-frame

;; (defmethod adopt-frame :before ((fm sdl2-port) (frame menu-frame))
;;   ;; Temporary kludge.
;;   (when (eq (slot-value frame 'climi::top) nil)
;;     (multiple-value-bind (x y)
;;         (xlib:query-pointer (clx-port-window (port fm)))
;;       (incf x 10)
;;       (setf (slot-value frame 'climi::left) x
;;             (slot-value frame 'climi::top) y))))

;; (defmethod adopt-frame :after ((fm clx-frame-manager) (frame menu-frame))
;;   (when (sheet-enabled-p (slot-value frame 'top-level-sheet))
;; (xlib:map-window (sheet-direct-xmirror (slot-value frame 'top-level-sheet)))))

;; (defmethod adopt-frame :after ((fm sdl2-port) (frame application-frame))
;;   (let ((sheet (slot-value frame 'top-level-sheet)))
;;     (let* ((top-level-sheet (frame-top-level-sheet frame))
;;            (mirror (sheet-direct-mirror top-level-sheet)))
;;       (case (clim-extensions:find-frame-type frame)
;;         (:override-redirect (setf (xlib:window-override-redirect mirror) :on))
;;         (:dialog (xlib:change-property mirror
;;                                        :_NET_WM_WINDOW_TYPE
;;                                        (list (xlib:intern-atom (xlib:window-display mirror) :_NET_WM_WINDOW_TYPE_DIALOG))
;;                                        :atom 32)))
;;       (multiple-value-bind (w h x y) (climi::frame-geometry* frame)
;;         (declare (ignore w h))
;;         (when (and x y)
;;           (setf (xlib:drawable-x mirror) x
;;                 (xlib:drawable-y mirror) y))
;;         (tell-window-manager-about-space-requirements top-level-sheet))
;;       ;; :structure-notify events were not yet turned on, turn them
;;       ;; on now, so that we get informed about the windows position
;;       ;; (and possibly size), when the window gets maped.
;;       (setf (xlib:window-event-mask mirror)
;;             (logior (xlib:window-event-mask mirror)
;;                     (xlib:make-event-mask :structure-notify)))
;;       ;; Care for calling-frame, be careful not to trip on missing bits
;;       (let* ((calling-frame (frame-calling-frame frame))
;;              (tls (and calling-frame (frame-top-level-sheet calling-frame)))
;;              (calling-mirror (and tls (sheet-xmirror tls))))
;;         (when calling-mirror
;;           (setf (xlib:transient-for mirror)
;;                 calling-mirror)))
;;       ;;
;;       (when (sheet-enabled-p sheet)
;;         (sdl2:create-window mirror)))))


;;    #:allocate-space
;;    #:destroy-mirror


(defmethod destroy-port :before ((port sdl2-port))
  (sdl2:quit))

;;    #:graft
;;    #:graft-height
;;    #:graft-width
;;    #:handle-repaint

(defmethod make-medium ((port sdl2-port) sheet)
  (make-instance 'basic-medium :sheet sheet))

(defmethod make-pane-1 ((fm sdl2-port) (frame application-frame) type &rest args)
  (apply #'make-instance type :frame frame :manager fm :port (port frame) args))

;;    #:medium-beep
;;    #:medium-buffering-output-p
;;    #:medium-clear-area
;;    #:medium-clipping-region
;;    #:medium-copy-area
;;    #:medium-draw-ellipse*
;;    #:medium-draw-line*
;;    #:medium-draw-lines*
;;    #:medium-draw-point*
;;    #:medium-draw-points*
;;    #:medium-draw-polygon*
;;    #:medium-draw-rectangle*
;;    #:medium-draw-rectangles*
;;    #:medium-draw-text*
;;    #:medium-finish-output
;;    #:medium-force-output
;;    #:medium-line-style
;;    #:medium-text-style
;;    #:note-space-requirements-changed
;;    #:pointer-button-state
;;    #:pointer-modifier-state
;;    #:pointer-position

(defgeneric desired-color (sheet))

(defmethod desired-color ((sheet permanent-medium-sheet-output-mixin))
  (medium-background sheet))

(defmethod desired-color ((sheet basic-pane))
  (let ((background (pane-background sheet)))
    (if (typep background 'color)
        background
        +white+)))

(defmethod desired-color (sheet)
  (declare (ignore sheet))
  +white+)

(defun round-coordinate (x)
  "Function used for rounding coordinates."
  
  ;; We use "mercantile rounding", instead of the CL round to nearest
  ;; even number, when in doubt.
  ;;
  ;; Reason: As the CLIM drawing model is specified, you quite often
  ;; want to operate with coordinates, which are multiples of 1/2. 
  ;; Using CL:ROUND gives you "random" results. Using "mercantile
  ;; rounding" gives you consistent results.
  ;;
  ;; Note that CLIM defines pixel coordinates to be at the corners,
  ;; while in X11 they are at the centers. We don't do much about the
  ;; discrepancy, but rounding up at half pixel boundaries seems to
  ;; work well.
  (floor (+ x .5)))

(defun realize-mirror-aux (port sheet
                           &key (title "")  (width 100) (height 100) (x 0) (y 0)
                             (border-width 0) (border 0)
                             (override-redirect :off)
                             (map t)
                             (backing-store :not-useful)
                             (save-under :off)
                             (event-mask `(:exposure 
                                           :key-press :key-release
                                           :button-press :button-release
                                           :enter-window :leave-window
                                           :structure-notify
                                           :pointer-motion
                                           :button-motion)))
  (declare (ignore border-width border override-redirect
                   backing-store save-under event-mask))
  (or (port-lookup-mirror port sheet)
      (destructuring-bind
            (&optional (x (round-coordinate x)) (y (round-coordinate y)))
          (transform-position
           (%sheet-mirror-transformation sheet) 0 0)
        (let* ((mirror-region
                (%sheet-mirror-region sheet))
               (window
                (sdl2:create-window :title title
                                    :w (if (not (null mirror-region))
                                           (round-coordinate
                                            (bounding-rectangle-width mirror-region))
                                           width)
                                    :h (if (not (null mirror-region))
                                           (round-coordinate
                                            (bounding-rectangle-height mirror-region))
                                           height)
                                    :x x
                                    :y y
                                    :flags '(:hidden))))
          (port-register-mirror port sheet window)
          (when (not (null map))
            (let* ((screen-surface
                    (sdl2:get-window-surface window))
                   (color
                    (multiple-value-call #'sdl2:map-rgb
                      (sdl2:surface-format screen-surface)
                      (color-rgb
                       (desired-color sheet)))))
              (sdl2:show-window window)
              (sdl2:fill-rect screen-surface nil color)
              (sdl2:update-window window)))
          window))))

 (defmethod realize-mirror ((port sdl2-port) (sheet basic-sheet))
   (realize-mirror-aux port sheet
                       :border-width 0
                       :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port sdl2-port) (sheet border-pane))
  (realize-mirror-aux port sheet
 		      :border-width 0
 		      :event-mask '(:exposure :structure-notify)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port sdl2-port) (sheet top-level-sheet-pane))
  (let ((q (compose-space sheet)))
    (realize-mirror-aux port sheet
                        :title (frame-pretty-name (pane-frame sheet))
                        :map nil
                        :width (round-coordinate (space-requirement-width q))
                        :height (round-coordinate (space-requirement-height q))
                        :event-mask '(:key-press :key-release))))
      ;;(setf (xlib:wm-hints window) (xlib:make-wm-hints :input :on))
      ;;(setf (xlib:wm-icon-name window) (frame-pretty-name frame))
      ;;(xlib:set-wm-class
      ;; window
      ;; (string-downcase (frame-name frame))
      ;; (string-capitalize (string-downcase (frame-name frame))))
      ;;(setf (xlib:wm-protocols window) `(:wm_delete_window))
      ;;(xlib:change-property window
      ;;                      :WM_CLIENT_LEADER (list (xlib:window-id window))
      ;;                      :WINDOW 32))))

(defmethod realize-mirror ((port sdl2-port) (sheet unmanaged-top-level-sheet-pane))
  (realize-mirror-aux port sheet
                      :override-redirect :on
                      :save-under :on
 		      :map nil
 		      :event-mask '(:structure-notify)))

(defmethod realize-mirror ((port sdl2-port) (sheet menu-button-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))

(defmethod realize-mirror ((port sdl2-port) (sheet clim-stream-pane))
  (realize-mirror-aux port sheet
		      :event-mask '(:exposure
				    :key-press :key-release
				    :button-press :button-release
				    :enter-window :leave-window
				    :structure-notify
				    :pointer-motion :pointer-motion-hint
				    :button-motion
				    :owner-grab-button)
                      :map (sheet-enabled-p sheet)))

(defmethod destroy-mirror ((port sdl2-port) sheet)
  (sdl2:destroy-window
   (port-lookup-mirror port sheet)))

;;    #:text-size
;;    #:text-style-ascent

;;(defmethod text-style-ascent (text-style (medium basic-medium))
;;  (let ((font (text-style-to-X-font (port medium) text-style)))
;;    (font-ascent font)))

;;    #:text-style-descent
;;    #:text-style-height
;;    #:text-style-mapping

;;(defmethod text-style-mapping ((port sdl2-port) text-style &optional character-set)
;;  (let ((

;;    #:text-style-width
;;    ;; Text selection protocol
;;    #:selection-owner
;;    #:selection-timestamp
;;    #:selection-event
;;    #:selection-clear-event
;;    #:selection-notify-event
;;    #:selection-request-event
;;    #:selection-event-requestor
;;    #:request-selection
;;    #:release-selection
;;    #:bind-selection
;;    #:send-selection
;;    #:get-selection-from-event
;;    ;; CLIM-EXTENSIONS
;;    #:medium-miter-limit

