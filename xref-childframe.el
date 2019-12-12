;;; xref-childframe.el --- child frame frontend for xref and eglot -*- lexical-binding: t; -*-;; Package-requires: ((emacs "26"))
;;; Commentary:

;; TODO
;; 2. integrate elisp backend

;;; Code:

(defgroup xref-childframe nil
  "Customization group for `xref-childframe'."
  :group 'eglot)

(defcustom xref-childframe-frame-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "J") 'xref-childframe-next-three-lines)
    (define-key map (kbd "K") 'xref-childframe-prev-three-lines)
    (define-key map (kbd "C-u") 'xref-childframe-scroll-up)
    (define-key map (kbd "C-d") 'xref-childframe-scroll-down)
    (define-key map (kbd "C-e") 'xref-childframe-next-xref)
    (define-key map (kbd "C-y") 'xref-childframe-prev-xref)
    (define-key map (kbd "<RET>") 'xref-childframe-switch-xref)
    (define-key map (kbd "<C-return>") 'xref-childframe-pop-xref)
    map)
  "Keymap for controlling help childframes."
  :type 'keymap
  :group 'xref-childframe)

(defcustom xref-childframe-mode-map (make-sparse-keymap)
  "Keymap for calling xref-childframe functions"
  :type 'keymap
  :group 'xref-childframe)

(defcustom xref-childframe-help-frame-height 30
  "Default height (in pixel) for the frame displaying help information."
  :type 'integer
  :group 'xref-childframe)

(defcustom xref-childframe-help-frame-width 80
  "Default width (in pixel) for the frame displaying help information."
  :type 'integer
  :group 'xref-childframe)

(defcustom xref-childframe-help-frame-position-fn
  #'xref-childframe-help-frame-default-position-fn
  "The function to set help frame position.  Should be a function that takes no argument, and return a cons cell of the X-Y coordinate of the frame.  e.g.
(lambda (&rest _) (0 . 0)) will always display the help frame at the top-left corner of the current frame."
  :type 'function
  :group 'xref-childframe)

(defcustom xref-childframe-xref-frame-height 30
  "Default height (in pixel) for the frame displaying xref."
  :type 'integer
  :group 'xref-childframe)

(defcustom xref-childframe-xref-frame-width 85
  "Default width (in pixel) for the frame displaying xref."
  :type 'integer
  :group 'xref-childframe)

(defcustom xref-childframe-xref-frame-position-fn
  #'xref-childframe-xref-frame-default-position-fn
  "The function to set xref frame position.  Should be a function that takes no argument and return a cons cell of the X-Y coordinate of the frame.  e.g.
(lambda (&rest _) (0 . 0)) will always display the help frame at the top-left corner of the current frame."
  :type 'function
  :group 'xref-childframe)

(define-minor-mode xref-childframe-mode
  "Minor mode to display eglot help, ref and def in childframes."
  nil nil xref-childframe-mode-map)

;;;###autoload
(defun xref-childframe-help ()
  "Display help for `symbol-at-point'."
  (interactive)
  (xref-childframe--create-frame
   xref-childframe-help-frame-position-fn
   xref-childframe-help-frame-width
   xref-childframe-help-frame-height
   #'xref-childframe--display-help))

;;;###autoload
(defun xref-childframe-definition ()
  "Display definition for `symbol-at-point'."
  (interactive)
  (xref-childframe--create-frame
   xref-childframe-xref-frame-position-fn
   xref-childframe-xref-frame-width
   xref-childframe-xref-frame-height
   #'xref-childframe--display-peek (xref-childframe--ref-at-point 'definitions)))

;;;###autoload
(defun xref-childframe-reference ()
  "Display references for `symbol-at-point'."
  (interactive)
  (xref-childframe--create-frame
   xref-childframe-xref-frame-position-fn
   xref-childframe-xref-frame-width
   xref-childframe-xref-frame-height
   #'xref-childframe--display-peek (xref-childframe--ref-at-point 'references)))

;;; ======================================================================
;; internal
;;; ======================================================================

(defvar xref-childframe--frame nil)

(defvar xref-childframe--content-window nil)

(defvar xref-childframe--control-window nil)

(defvar xref-childframe--current-xref nil)

(defconst xref-childframe--content-buffer " *xref-childframe-content*")

(defconst xref-childframe--control-buffer " *xref-childframe-control*")

(defconst xref-childframe--init-parameters
  '((parent-frame . (window-frame))
    (skip-taskbar . t)
    (minibuffer . nil)
    (visibility . nil)
    (left-fringe . 3)
    (right-fringe . 3)
    (internal-border-width . 1)
    (vertical-scroll-bars . right)
    (scroll-bar-width . 8)
    (horizontal-scroll-bars . nil)
    (undecorated . t)
    (header-line-format . nil)
    (mode-line-format . nil)
    (unsplittable . t)
    (bottom-divider-width . 2)))

(defvar-local xref-childframe--restore-keymap-fn nil)

;;; ===============================
;;  frame creation
;;; ===============================

(defun xref-childframe--create-frame (position width height display-fun &rest args)
  "Create a child frame at POSITION with WIDTH and HEIGHT.  After child frame is created, call DISPLAY-FUN with ARGS in the child frame to generate contents to be displayed in the child frame."
  (setq xref-childframe--frame
        (make-frame (append xref-childframe--init-parameters
                            `((width . ,width)
                              (height . ,height)))))

  (let ((pos (if (functionp position)
                 (funcall position)
               position)))
    ;; follow snails.el
    (with-selected-frame xref-childframe--frame
      ;; Delete other window first, make sure only one window in frame.
      (delete-other-windows)

      ;; Disable menu
      (set-frame-parameter xref-childframe--frame 'menu-bar-lines 0)

      ;; move frame to desirable position
      (set-frame-position xref-childframe--frame (car pos) (cdr pos))
      (set-face-background 'internal-border "gray80" xref-childframe--frame)

      ;; call display function to display content
      (setq xref-childframe--content-window (selected-window))
      (apply display-fun args)

      ;; deal with buffer-local-variables
      (with-current-buffer xref-childframe--content-buffer
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil))))

  ;; finally show frame
  (make-frame-visible xref-childframe--frame))

;;; ===============================
;;  help-at-point
;;; ===============================

(defun xref-childframe--display-help (&rest _)
  "Display help at point."
  (with-selected-window xref-childframe--content-window
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (when (seq-empty-p contents) (eglot--error "No hover info here"))
      (let ((blurb (eglot--hover-info contents range)))
        (with-current-buffer (get-buffer-create xref-childframe--content-buffer)
          (erase-buffer)
          (insert blurb)
          (goto-char 1)

          (setq xref-childframe--restore-keymap-fn
                (set-transient-map
                 xref-childframe-frame-map t #'xref-childframe-hide)))))

    (switch-to-buffer xref-childframe--content-buffer)))

;;; ===============================
;;  def/ref-at-point
;;; ===============================

(defun xref-childframe--ref-at-point (kind)
  "Get a list of xref references item of KIND (e.g., definitions, references, etc.)."
  (let* ((id (xref-backend-identifier-at-point 'eglot))
         (xrefs (funcall (intern (format "xref-backend-%s" kind))
                         (xref-find-backend)
                         id)))
    (if xrefs
        ;; first remove the distracting highlight in the summary of xrefs.
        (progn
          (seq-doseq (xref xrefs)
            (let ((summ (xref-item-summary xref)))
              (font-lock--remove-face-from-text-property
               0 (length summ) 'face 'highlight summ)))
          xrefs)
      (user-error (format "No %s found at point" kind)))))

(defun xref-childframe--peek (xref)
  "Peek XREF."

  ;; debug
  ;; (unless (xref-item-p xref)
  ;;   (user-error "argument is not an xref-item"))

  (with-selected-window xref-childframe--content-window
    (let* ((xref-loc (xref-item-location xref))
           (xref-file (xref-location-group xref-loc))
           (xref-line (xref-location-line xref-loc)))
      (setq xref-childframe--current-xref xref)

      (with-current-buffer (get-buffer-create xref-childframe--content-buffer)
        (erase-buffer)
        (insert-file-contents xref-file)
        (delay-mode-hooks
          (let ((inhibit-message t)
                (buffer-file-name xref-file))
            (set-auto-mode)
            (display-line-numbers-mode)
            (setq-local display-line-numbers t)
            (turn-on-font-lock)
            (font-lock-ensure)))

        (setq xref-childframe--restore-keymap-fn
              (set-transient-map
               xref-childframe-frame-map t #'xref-childframe-hide)))

      (switch-to-buffer xref-childframe--content-buffer)
      ;; FIXME how to go from line number to loc?
      (goto-char 1)
      (line-move (1- xref-line))

      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (add-face-text-property beg end 'region t)
        (line-move (* 2 (/ xref-childframe-xref-frame-height 3)) 'noerror)))))

(defun xref-childframe--display-peek (xrefs)
  "Disply peeks for `symbol-at-point'."
  (xref-childframe--peek (car xrefs))

  (when (cdr xrefs)

    ;; create control window
    (setq xref-childframe--control-window
          (split-window-vertically -4))
    (other-window 1)

    (let ((xref-alist (xref--analyze xrefs)))
      (with-current-buffer (get-buffer-create xref-childframe--control-buffer)
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (erase-buffer)
          (xref--insert-xrefs xref-alist))))

    (switch-to-buffer xref-childframe--control-buffer)

    ;; format control panel
    (setq mode-line-format nil)
    ;; create indicator
    (goto-char (point-min))
    (let* ((_ (xref--search-property 'xref-item))
           (beg (line-beginning-position))
           (end (line-end-position)))
      (xref-childframe--select-xref beg end))))

;;; ===============================
;; commands
;;; ===============================

(defun xref-childframe-hide ()
  "Hide childframe."
  (interactive)
  ;; deactivate keymaps
  (when xref-childframe--restore-keymap-fn
    (let ((fn xref-childframe--restore-keymap-fn))
      (setq xref-childframe--restore-keymap-fn nil)
      (funcall fn)))

  (when xref-childframe--frame
    (delete-frame xref-childframe--frame))
  (setq xref-childframe--frame nil))

(defun xref-childframe-command (fn &rest args)
  (with-selected-window xref-childframe--content-window
    (when (functionp fn)
      (funcall fn args))))

(defun xref-childframe-next-three-lines ()
  "Move down three lines."
  (interactive)
  (xref-childframe-command #'scroll-up 3))

(defun xref-childframe-prev-three-lines ()
  "Move up three lines."
  (interactive)
  (xref-childframe-command #'scroll-down 3))

(defun xref-childframe-scroll-up ()
  "Move up half page."
  (interactive)
  (xref-childframe-command #'scroll-down
                            (max 1 (/ (1- (window-height (selected-window))) 2))))

(defun xref-childframe-scroll-down ()
  "Move down half page."
  (interactive)
  (xref-childframe-command #'scroll-up
                            (max 1 (/ (1- (window-height (selected-window))) 2))))

(defun xref-childframe-switch-xref ()
  "Hide the childframe, switch to `xref-childframe--current-xref'."
  (interactive)
  (xref--pop-to-location xref-childframe--current-xref nil)
  (setq xref-childframe--current-xref nil)
  (xref-childframe-hide))

(defun xref-childframe-pop-xref ()
  "Hide the childframe, pop to `xref-childframe--current-xref' in another window."
  (interactive)
  (xref--pop-to-location xref-childframe--current-xref 'window)
  (setq xref-childframe--current-xref nil)
  (xref-childframe-hide))

(defun xref-childframe-move-xref (&optional prev)
  "Move to next xref.  If PREV is non-nil, move to previous xref."
  (with-current-buffer xref-childframe--control-buffer
    (let* ((xref (xref--search-property 'xref-item prev))
           (beg (line-beginning-position))
           (end (line-end-position)))
      (xref-childframe--select-xref beg end)

      ;; update xref
      (setq xref-childframe--current-xref xref)

      (with-selected-window xref-childframe--control-window
        (goto-char beg)
        (recenter nil))

      ;; display xref in the content window
      (xref-childframe--peek xref))))

(defun xref-childframe-next-xref ()
  (interactive)
  (xref-childframe-move-xref))

(defun xref-childframe-prev-xref ()
  (interactive)
  (xref-childframe-move-xref 'prev))

;;; ===============================
;; misc
;;; ===============================

(defun xref-childframe--flatten-list (args)
  "Flatten any lists within ARGS, so that there are no sublists.  Ported from eshell."
  (let ((new-list (list t)))
    (dolist (a args)
      (if (and (listp a)
	             (listp (cdr a)))
	        (nconc new-list (xref-childframe--flatten-list a))
	      (nconc new-list (list a))))
    (cdr new-list)))

(defun xref-childframe--select-xref (beg end)
  "Create overlay of summary of xref between BEG and END in order to set it apart to other xrefs."
  (mapc 'delete-overlay (xref-childframe-flatten-list (overlay-lists)))
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face '(:box t))))

(defun xref-childframe-help-frame-default-position-fn ()
  (let ((frame-edges (frame-edges)))
    (if (eq (car (window-edges)) 0)
        ;; current window on the left, display at the top right corner
        (cons (nth 2 frame-edges) (nth 1 frame-edges))
      (cons (nth 0 frame-edges) (nth 1 frame-edges)))))

(defun xref-childframe-xref-frame-default-position-fn ()
  (let* ((symbol-at-point-pos (save-excursion
                                (beginning-of-thing 'symbol)
                                (window-absolute-pixel-position)))
         (x (car symbol-at-point-pos))
         (y (+ (default-font-height) (cdr symbol-at-point-pos)))
         (cf-width (frame-pixel-width xref-childframe--frame))
         (cf-height (frame-pixel-height xref-childframe--frame))
         (cf-right-edge (+ x cf-width))
         (cf-bottom-edge (+ y cf-height))
         ;; parent frame width/height need to consider the possibility of secondary
         ;; monitor
         (frame-edges (frame-edges))
         (f-width (+ (frame-pixel-width) (nth 0 frame-edges)))
         (f-height (+ (frame-pixel-height) (nth 1 frame-edges))))
    (cond
     ;; case 1: childframe is too wide, note that this could only happen on the right
     ;; edge with reasonable value of `xref-childframe-xref-frame-width' (e.g., < 100),
     ;; shifting the whole child frame to the left will not cause problem
     ((>= cf-right-edge f-width)
      (cons (- x (- cf-right-edge f-width) 10) y))
     ;; case 2: childframe is too tall, note that this could only happen on the bottom
     ;; edge different from case 1, we do not want to simply shift the whole childframe
     ;; up, which will cover the point position of the parent frame. Instead, we scroll
     ;; the parent window up and generate enough room for the child frame
     ((>= cf-bottom-edge f-height)
      (let* ((pixel-needed (- cf-bottom-edge f-height))
             ;; +3 - takes into account the minibuffer and the mode-line
             (line-needed (+ 3 (/ (abs pixel-needed) (default-font-height)))))
        (evil-scroll-line-down line-needed)
        ;; now we should be in case 1
        (xref-childframe-xref-frame-default-position-fn)))

     ;; case 3: well within the premise of the current frame
     (t (cons x y)))))

(provide 'xref-childframe)
;;; xref-childframe.el ends here
