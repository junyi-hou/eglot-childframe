;;; eglot-childframe.el --- child frame frontend for eglot -*- lexical-binding: t; -*-;; Package-requires: ((emacs "26"))
;;; Commentary:

;; TODO
;; 1. multi-monitor support
;; 2. integrate elisp backend

;;; Code:

(defgroup eglot-childframe nil
  "Customization group for `eglot-childframe'."
  :group 'eglot)

(defcustom eglot-childframe-frame-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "J") 'eglot-childframe-next-three-lines)
    (define-key map (kbd "K") 'eglot-childframe-prev-three-lines)
    (define-key map (kbd "C-u") 'eglot-childframe-scroll-up)
    (define-key map (kbd "C-d") 'eglot-childframe-scroll-down)
    (define-key map (kbd "C-e") 'eglot-childframe-next-xref)
    (define-key map (kbd "C-y") 'eglot-childframe-prev-xref)
    (define-key map (kbd "<RET>") 'eglot-childframe-switch-xref)
    (define-key map (kbd "<C-return>") 'eglot-childframe-pop-xref)
    map)
  "Keymap for controlling help childframes."
  :type 'keymap
  :group 'eglot-childframe)

(defcustom eglot-childframe-mode-map (make-sparse-keymap)
  "Keymap for calling eglot-childframe functions"
  :type 'keymap
  :group 'eglot-childframe)

(defcustom eglot-childframe-help-frame-height 30
  "Default height (in pixel) for the frame displaying help information."
  :type 'integer
  :group 'eglot-childframe)

(defcustom eglot-childframe-help-frame-width 80
  "Default width (in pixel) for the frame displaying help information."
  :type 'integer
  :group 'eglot-childframe)

(defcustom eglot-childframe-help-frame-position-fn
  #'eglot-childframe-help-frame-default-position-fn
  "The function to set help frame position.  Should be a function that takes no argument, and return a cons cell of the X-Y coordinate of the frame.  e.g.
(lambda (&rest _) (0 . 0)) will always display the help frame at the top-left corner of the current frame."
  :type 'function
  :group 'eglot-childframe)

(defcustom eglot-childframe-xref-frame-height 30
  "Default height (in pixel) for the frame displaying xref."
  :type 'integer
  :group 'eglot-childframe)

(defcustom eglot-childframe-xref-frame-width 85
  "Default width (in pixel) for the frame displaying xref."
  :type 'integer
  :group 'eglot-childframe)

(defcustom eglot-childframe-xref-frame-position-fn
  #'eglot-childframe-xref-frame-default-position-fn
  "The function to set xref frame position.  Should be a function that takes no argument and return a cons cell of the X-Y coordinate of the frame.  e.g.
(lambda (&rest _) (0 . 0)) will always display the help frame at the top-left corner of the current frame."
  :type 'function
  :group 'eglot-childframe)

(define-minor-mode eglot-childframe-mode
  "Minor mode to display eglot help, ref and def in childframes."
  nil nil eglot-childframe-mode-map)

;;;###autoload
(defun eglot-childframe-help ()
  "Display help for `symbol-at-point'."
  (interactive)
  (eglot-childframe--create-frame
   eglot-childframe-help-frame-position-fn
   eglot-childframe-help-frame-width
   eglot-childframe-help-frame-height
   #'eglot-childframe--display-help))

;;;###autoload
(defun eglot-childframe-definition ()
  "Display definition for `symbol-at-point'."
  (interactive)
  (eglot-childframe--create-frame
   eglot-childframe-xref-frame-position-fn
   eglot-childframe-xref-frame-width
   eglot-childframe-xref-frame-height
   #'eglot-childframe--display-peek (eglot-childframe--ref-at-point 'definitions)))

;;;###autoload
(defun eglot-childframe-reference ()
  "Display references for `symbol-at-point'."
  (interactive)
  (eglot-childframe--create-frame
   eglot-childframe-xref-frame-position-fn
   eglot-childframe-xref-frame-width
   eglot-childframe-xref-frame-height
   #'eglot-childframe--display-peek (eglot-childframe--ref-at-point 'references)))

;;; ======================================================================
;; internal
;;; ======================================================================

(defvar eglot-childframe--frame nil)

(defvar eglot-childframe--content-window nil)

(defvar eglot-childframe--control-window nil)

(defvar eglot-childframe--current-xref nil)

(defconst eglot-childframe--content-buffer " *eglot-childframe-content*")

(defconst eglot-childframe--control-buffer " *eglot-childframe-control*")

(defconst eglot-childframe--init-parameters
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

(defvar-local eglot-childframe--restore-keymap-fn nil)

;;; ===============================
;;  frame creation
;;; ===============================

(defun eglot-childframe--create-frame (position width height display-fun &rest args)
  "Create a child frame at POSITION with WIDTH and HEIGHT.  After child frame is created, call DISPLAY-FUN with ARGS in the child frame to generate contents to be displayed in the child frame."
  (setq eglot-childframe--frame
        (make-frame (append eglot-childframe--init-parameters
                            `((width . ,width)
                              (height . ,height)))))

  (let ((pos (if (functionp position)
                 (funcall position)
               position)))
    ;; follow snails.el
    (with-selected-frame eglot-childframe--frame
      ;; Delete other window first, make sure only one window in frame.
      (delete-other-windows)

      ;; Disable menu
      (set-frame-parameter eglot-childframe--frame 'menu-bar-lines 0)

      ;; move frame to desirable position
      (set-frame-position eglot-childframe--frame (car pos) (cdr pos))
      (set-face-background 'internal-border "gray80" eglot-childframe--frame)

      ;; call display function to display content
      (setq eglot-childframe--content-window (selected-window))
      (apply display-fun args)

      ;; deal with buffer-local-variables
      (with-current-buffer eglot-childframe--content-buffer
        (setq-local cursor-type nil)
        (setq-local cursor-in-non-selected-windows nil)
        (setq-local mode-line-format nil)
        (setq-local header-line-format nil))))

  ;; finally show frame
  (make-frame-visible eglot-childframe--frame))

;;; ===============================
;;  help-at-point
;;; ===============================

(defun eglot-childframe--display-help (&rest _)
  "Display help at point."
  (with-selected-window eglot-childframe--content-window
    (eglot--dbind ((Hover) contents range)
        (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                         (eglot--TextDocumentPositionParams))
      (when (seq-empty-p contents) (eglot--error "No hover info here"))
      (let ((blurb (eglot--hover-info contents range)))
        (with-current-buffer (get-buffer-create eglot-childframe--content-buffer)
          (erase-buffer)
          (insert blurb)
          (goto-char 1)

          (setq eglot-childframe--restore-keymap-fn
                (set-transient-map
                 eglot-childframe-frame-map t #'eglot-childframe-hide)))))

    (switch-to-buffer eglot-childframe--content-buffer)))

;;; ===============================
;;  def/ref-at-point
;;; ===============================

(defun eglot-childframe--ref-at-point (kind)
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

(defun eglot-childframe--peek (xref)
  "Peek XREF."

  ;; debug
  ;; (unless (xref-item-p xref)
  ;;   (user-error "argument is not an xref-item"))

  (with-selected-window eglot-childframe--content-window
    (let* ((xref-loc (xref-item-location xref))
           (xref-file (xref-location-group xref-loc))
           (xref-line (xref-location-line xref-loc)))
      (setq eglot-childframe--current-xref xref)

      (with-current-buffer (get-buffer-create eglot-childframe--content-buffer)
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

        (setq eglot-childframe--restore-keymap-fn
              (set-transient-map
               eglot-childframe-frame-map t #'eglot-childframe-hide)))

      (switch-to-buffer eglot-childframe--content-buffer)
      ;; FIXME how to go from line number to loc?
      (goto-char 1)
      (line-move (1- xref-line))

      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (add-face-text-property beg end 'region t)
        (line-move (* 2 (/ eglot-childframe-xref-frame-height 3)) 'noerror)))))

(defun eglot-childframe--display-peek (xrefs)
  "Disply peeks for `symbol-at-point'."
  (eglot-childframe--peek (car xrefs))

  (when (cdr xrefs)

    ;; create control window
    (setq eglot-childframe--control-window
          (split-window-vertically -4))
    (other-window 1)

    (let ((xref-alist (xref--analyze xrefs)))
      (with-current-buffer (get-buffer-create eglot-childframe--control-buffer)
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (erase-buffer)
          (xref--insert-xrefs xref-alist))))

    (switch-to-buffer eglot-childframe--control-buffer)

    ;; format control panel
    (setq mode-line-format nil)
    ;; create indicator
    (goto-char (point-min))
    (let* ((_ (xref--search-property 'xref-item))
           (beg (line-beginning-position))
           (end (line-end-position)))
      (eglot-childframe--select-xref beg end))))

;;; ===============================
;; commands
;;; ===============================

(defun eglot-childframe-hide ()
  "Hide childframe."
  (interactive)
  ;; deactivate keymaps
  (when eglot-childframe--restore-keymap-fn
    (let ((fn eglot-childframe--restore-keymap-fn))
      (setq eglot-childframe--restore-keymap-fn nil)
      (funcall fn)))

  (when eglot-childframe--frame
    (delete-frame eglot-childframe--frame))
  (setq eglot-childframe--frame nil))

(defun eglot-childframe-command (fn &rest args)
  (with-selected-window eglot-childframe--content-window
    (when (functionp fn)
      (funcall fn args))))

(defun eglot-childframe-next-three-lines ()
  "Move down three lines."
  (interactive)
  (eglot-childframe-command #'scroll-up 3))

(defun eglot-childframe-prev-three-lines ()
  "Move up three lines."
  (interactive)
  (eglot-childframe-command #'scroll-down 3))

(defun eglot-childframe-scroll-up ()
  "Move up half page."
  (interactive)
  (eglot-childframe-command #'scroll-down
                            (max 1 (/ (1- (window-height (selected-window))) 2))))

(defun eglot-childframe-scroll-down ()
  "Move down half page."
  (interactive)
  (eglot-childframe-command #'scroll-up
                            (max 1 (/ (1- (window-height (selected-window))) 2))))

(defun eglot-childframe-switch-xref ()
  "Hide the childframe, switch to `eglot-childframe--current-xref'."
  (interactive)
  (xref--pop-to-location eglot-childframe--current-xref nil)
  (setq eglot-childframe--current-xref nil)
  (eglot-childframe-hide))

(defun eglot-childframe-pop-xref ()
  "Hide the childframe, pop to `eglot-childframe--current-xref' in another window."
  (interactive)
  (xref--pop-to-location eglot-childframe--current-xref 'window)
  (setq eglot-childframe--current-xref nil)
  (eglot-childframe-hide))

(defun eglot-childframe-move-xref (&optional prev)
  "Move to next xref.  If PREV is non-nil, move to previous xref."
  (with-current-buffer eglot-childframe--control-buffer
    (let* ((xref (xref--search-property 'xref-item prev))
           (beg (line-beginning-position))
           (end (line-end-position)))
      (eglot-childframe--select-xref beg end)

      ;; update xref
      (setq eglot-childframe--current-xref xref)

      (with-selected-window eglot-childframe--control-window
        (goto-char beg)
        (recenter nil))

      ;; display xref in the content window
      (eglot-childframe--peek xref))))

(defun eglot-childframe-next-xref ()
  (interactive)
  (eglot-childframe-move-xref))

(defun eglot-childframe-prev-xref ()
  (interactive)
  (eglot-childframe-move-xref 'prev))

;;; ===============================
;; misc
;;; ===============================

(defun eglot-childframe--flatten-list (args)
  "Flatten any lists within ARGS, so that there are no sublists.  Ported from eshell."
  (let ((new-list (list t)))
    (dolist (a args)
      (if (and (listp a)
	             (listp (cdr a)))
	        (nconc new-list (eglot-childframe--flatten-list a))
	      (nconc new-list (list a))))
    (cdr new-list)))

(defun eglot-childframe--select-xref (beg end)
  "Create overlay of summary of xref between BEG and END in order to set it apart to other xrefs."
  (mapc 'delete-overlay (eglot-childframe-flatten-list (overlay-lists)))
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face '(:box t))))

(defun eglot-childframe-help-frame-default-position-fn ()
  (let ((frame-edges (frame-edges)))
    (if (eq (car (window-edges)) 0)
        ;; current window on the left, display at the top right corner
        (cons (nth 2 frame-edges) (nth 1 frame-edges))
      (cons (nth 0 frame-edges) (nth 1 frame-edges)))))

(defun eglot-childframe-xref-frame-default-position-fn ()
  (let* ((symbol-at-point-pos (save-excursion
                                (beginning-of-thing 'symbol)
                                (window-absolute-pixel-position)))
         (x (car symbol-at-point-pos))
         (y (+ (default-font-height) (cdr symbol-at-point-pos)))
         (cf-width (frame-pixel-width eglot-childframe--frame))
         (cf-height (frame-pixel-height eglot-childframe--frame))
         (cf-right-edge (+ x cf-width))
         (cf-bottom-edge (+ y cf-height))
         ;; parent frame width/height need to consider the possibility of secondary
         ;; monitor
         (frame-edges (frame-edges))
         (f-width (+ (frame-pixel-width) (nth 0 frame-edges)))
         (f-height (+ (frame-pixel-height) (nth 1 frame-edges))))
    (cond
     ;; case 1: childframe is too wide, note that this could only happen on the right
     ;; edge with reasonable value of `eglot-childframe-xref-frame-width' (e.g., < 100),
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
        (eglot-childframe-xref-frame-default-position-fn)))

     ;; case 3: well within the premise of the current frame
     (t (cons x y)))))

(provide 'eglot-childframe)
;;; eglot-childframe.el ends here
