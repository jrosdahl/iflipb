;;; icycleb --- interactively cycle between recently visited buffers

;; Copyright (C) 2007-2009 Joel Rosdahl
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the author nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;;
;; ============================================================================
;;
;; icycleb lets you cycle between recently visited buffers in a way
;; that resembles what Alt-(Shift-)TAB does in Microsoft Windows and
;; other graphical window managers. It provides two commands:
;; icycleb-next-buffer and icycleb-previous-buffer.
;;
;; icycleb-next-buffer behaves like Alt-TAB: it switches to the
;; previously used buffer, just like "C-x b RET" (or C-M-l in XEmacs).
;; Another consecutive call to icycleb-next-buffer switches to the
;; next buffer in the buffer list, and so on. While cycling, the names
;; of the most recent buffers are displayed in the minibuffer, and the
;; currently visited buffer is marked with a bold face and surrounded
;; by square brackets.
;;
;; icycleb-previous-buffer behaves like Alt-Shift-TAB: it walks
;; backwards in the buffer list.
;;
;; To load icycleb, store icycleb.el in your Emacs library path and
;; put
;;
;;     (load "icycleb")
;;
;; in your .emacs.
;;
;; This file does not install any key bindings for the two commands. I
;; personally use M-h and M-H (i.e., M-S-h) since I don't use the
;; standard binding of M-h (mark-paragraph) and M-h is quick to press.
;; To install icycleb with M-h and M-H as keyboard bindings, put
;; something like this in your .emacs:
;;
;;     (global-set-key (kbd "M-h") 'icycleb-next-buffer)
;;     (global-set-key (kbd "M-H") 'icycleb-previous-buffer)
;;
;; Another alternative is to use C-tab and C-S-tab:
;;
;;     (if (featurep 'xemacs)
;;         (global-set-key (kbd "<C-iso-left-tab>") 'icycleb-previous-buffer)
;;       (global-set-key (kbd "<C-S-iso-lefttab>") 'icycleb-previous-buffer))
;;     (global-set-key (kbd "<C-tab>") 'icycleb-next-buffer)
;;
;; Or perhaps use functions keys like F9 and F10:
;;
;;     (global-set-key (kbd "<f9>")  'icycleb-previous-buffer)
;;     (global-set-key (kbd "<f10>") 'icycleb-next-buffer)
;;
;; icycleb was inspired by cycle-buffer.el
;; <http://kellyfelkins.org/pub/cycle-buffer.el>. cycle-buffer.el has
;; some more features, but doesn't quite behave like I want, so I
;; wrote my own simple replacement.
;;
;; Other alternatives to icycleb include:
;;
;;   * iswitchb-mode
;;   * ido-mode
;;   * icicles
;;   * buffer-stack
;;
;; /Joel Rosdahl <joel@rosdahl.net>

(defvar icycleb-current-buffer-index 0
  "Index of the currently displayed buffer in the buffer list.")
(defvar icycleb-saved-buffers nil
  "Saved buffer list state; the original order of buffers to the left
of icycleb-current-buffer-index.")

(defun icycleb-first-n (n list)
  "Returns the first n elements of a list."
  (butlast list (- (length list) n)))

(defun icycleb-filter (elements pred)
  "Returns elements that satisfy a predicate."
  (let ((result nil))
    (while elements
      (let ((elem (car elements))
            (rest (cdr elements)))
        (when (funcall pred elem)
          (setq result (cons elem result)))
        (setq elements rest)))
    (nreverse result)))

(defun icycleb-interesting-buffer-p (name)
  "Decides whether a buffer name should be included in the displayed
buffer list."
  (not (eq (string-to-char (buffer-name name)) ?\ )))

(defun icycleb-interesting-buffers ()
  "Returns buffers that should be included in the displayed buffer
list."
  (icycleb-filter (buffer-list) 'icycleb-interesting-buffer-p))

(defun icycleb-first-icycleb-buffer-switch-command ()
  "Determines whether this is the first invocation of
icycleb-next-buffer or icycleb-previous-buffer this round."
  (not (or (eq last-command 'icycleb-next-buffer)
           (eq last-command 'icycleb-previous-buffer))))

(defun icycleb-restore-buffers ()
  "Helper function that restores the buffer list to the original state."
  (mapc 'switch-to-buffer (reverse icycleb-saved-buffers)))

(defun icycleb-format-buffer (current-buffer buffer)
  "Format a buffer name for inclusion in the buffer list in the
minibuffer."
  (let ((name (buffer-name buffer)))
    (when (eq current-buffer buffer)
      (setq name (format "[%s]" name))
      (add-text-properties 1 (1- (length name)) '(face bold) name))
    name))
(defun icycleb-format-buffers (current-buffer buffers)
  "Format buffer names for displaying them in the minibuffer."
  (truncate-string-to-width
   (mapconcat
    (lambda (buffer)
      (icycleb-format-buffer current-buffer buffer))
    buffers
    " ")
   (1- (window-width (minibuffer-window)))))

(defun icycleb-select-buffer (index)
  "Helper function that shows the buffer with a given index."
  (icycleb-restore-buffers)
  (setq icycleb-saved-buffers nil)
  (let* ((buffers (icycleb-interesting-buffers))
         (current-buffer (nth index buffers)))
    (setq icycleb-current-buffer-index index)
    (setq icycleb-saved-buffers (icycleb-first-n index buffers))
    (message (icycleb-format-buffers current-buffer buffers))
    (switch-to-buffer current-buffer)))

(defun icycleb-next-buffer ()
  "Switch to the next buffer in the buffer list. Consecutive invocations
switches to less recent buffers in the buffer list."
  (interactive)
  (when (icycleb-first-icycleb-buffer-switch-command)
    (setq icycleb-current-buffer-index 0)
    (setq icycleb-saved-buffers nil))
  (if (= icycleb-current-buffer-index
         (1- (length (icycleb-interesting-buffers))))
      (message "No more buffers.")
    (icycleb-select-buffer (1+ icycleb-current-buffer-index)))
  (setq last-command 'icycleb-next-buffer))

(defun icycleb-previous-buffer ()
  "Switch to the previous buffer in the buffer list. Consecutive invocations
switches to more recent buffers in the buffer list."
  (interactive)
  (when (icycleb-first-icycleb-buffer-switch-command)
    (setq icycleb-current-buffer-index 0)
    (setq icycleb-saved-buffers nil))
  (if (= icycleb-current-buffer-index 0)
      (message "You are already looking at the top buffer.")
    (icycleb-select-buffer (1- icycleb-current-buffer-index)))
  (setq last-command 'icycleb-previous-buffer))
