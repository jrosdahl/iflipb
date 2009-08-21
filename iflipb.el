;;; iflipb -- interactively flip between recently visited buffers
;;
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
;; iflipb lets you flip between recently visited buffers in a way that
;; resembles what Alt-(Shift-)TAB does in Microsoft Windows and other
;; graphical window managers. However, iflipb by design doesn't treat
;; the buffer list as a ring; when you have flipped to the last buffer
;; and continue, you don't get to the first buffer again.
;;
;; iflipb provides two commands: iflipb-next-buffer and
;; iflipb-previous-buffer.
;;
;; iflipb-next-buffer behaves like Alt-TAB: it switches to the
;; previously used buffer, just like "C-x b RET" (or C-M-l in XEmacs).
;; Another consecutive call to iflipb-next-buffer switches to the next
;; buffer in the buffer list, and so on. While flipping, the names of
;; the most recent buffers are displayed in the minibuffer, and the
;; currently visited buffer is surrounded by square brackets and
;; marked with a bold face and.
;;
;; iflipb-previous-buffer behaves like Alt-Shift-TAB: it walks
;; backwards in the buffer list.
;;
;; To load iflipb, store iflipb.el in your Emacs library path and put
;;
;;   (require 'iflipb)
;;
;; in your .emacs or equivalent.
;;
;; This file does not install any key bindings for the two commands. I
;; personally use M-h and M-H (i.e., M-S-h) since I don't use the
;; standard binding of M-h (mark-paragraph) and M-h is quick and easy
;; to press. To install iflipb with M-h and M-H as keyboard bindings,
;; put something like this in your .emacs:
;;
;;   (global-set-key (kbd "M-h") 'iflipb-next-buffer)
;;   (global-set-key (kbd "M-H") 'iflipb-previous-buffer)
;;
;; Another alternative is to use C-tab and C-S-tab:
;;
;;   (global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
;;   (global-set-key
;;    (if (featurep 'xemacs) (kbd "<C-iso-left-tab>") (kbd "<C-S-iso-lefttab>"))
;;     'iflipb-previous-buffer)
;;
;; Or perhaps use functions keys like F9 and F10:
;;
;;   (global-set-key (kbd "<f10>") 'iflipb-next-buffer)
;;   (global-set-key (kbd "<f9>")  'iflipb-previous-buffer)
;;
;; iflipb was inspired by cycle-buffer.el
;; <http://kellyfelkins.org/pub/cycle-buffer.el>. cycle-buffer.el has
;; some more features, but doesn't quite behave like I want, so I
;; wrote my own simple replacement.
;;
;; Other alternatives to iflipb include:
;;
;;   * iswitchb-mode
;;   * ido-mode
;;   * icicles
;;   * buffer-stack
;;
;; Have fun!
;;
;; /Joel Rosdahl <joel@rosdahl.net>
;;

(defvar iflipb-boring-buffer-filter "^\\( \\|\\*\\)"
  "*This variable may be either a regexp or a function. If it's a
regexp, it describes buffer names to exclude from the buffer list. If
it's a function, the function will get a buffer name as an argument. A
return value of nil from the function means include and non-nil means
exclude.")
(defvar iflipb-current-buffer-index 0
  "Index of the currently displayed buffer in the buffer list.")
(defvar iflipb-saved-buffers nil
  "Saved buffer list state; the original order of buffers to the left
of iflipb-current-buffer-index.")

(defun iflipb-first-n (n list)
  "Returns the first n elements of a list."
  (butlast list (- (length list) n)))

(defun iflipb-filter (elements pred)
  "Returns elements that satisfy a predicate."
  (let ((result nil))
    (while elements
      (let ((elem (car elements))
            (rest (cdr elements)))
        (when (funcall pred elem)
          (setq result (cons elem result)))
        (setq elements rest)))
    (nreverse result)))

(defun iflipb-interesting-buffer-p (buffer)
  "Decides whether a buffer name should be included in the displayed
buffer list."
  (not
   (let ((name (buffer-name buffer)))
     (if (functionp iflipb-boring-buffer-filter)
         (funcall iflipb-boring-buffer-filter name)
       (string-match iflipb-boring-buffer-filter name)))))

(defun iflipb-interesting-buffers ()
  "Returns buffers that should be included in the displayed buffer
list."
  (iflipb-filter (buffer-list) 'iflipb-interesting-buffer-p))

(defun iflipb-first-iflipb-buffer-switch-command ()
  "Determines whether this is the first invocation of
iflipb-next-buffer or iflipb-previous-buffer this round."
  (not (or (eq last-command 'iflipb-next-buffer)
           (eq last-command 'iflipb-previous-buffer))))

(defun iflipb-restore-buffers ()
  "Helper function that restores the buffer list to the original state."
  (mapc 'switch-to-buffer (reverse iflipb-saved-buffers)))

(defun iflipb-format-buffer (current-buffer buffer)
  "Format a buffer name for inclusion in the buffer list in the
minibuffer."
  (let ((name (buffer-name buffer)))
    (when (eq current-buffer buffer)
      (setq name (format "[%s]" name))
      (add-text-properties 1 (1- (length name)) '(face bold) name))
    name))
(defun iflipb-format-buffers (current-buffer buffers)
  "Format buffer names for displaying them in the minibuffer."
  (truncate-string-to-width
   (mapconcat
    (lambda (buffer)
      (iflipb-format-buffer current-buffer buffer))
    buffers
    " ")
   (1- (window-width (minibuffer-window)))))

(defun iflipb-select-buffer (index)
  "Helper function that shows the buffer with a given index."
  (iflipb-restore-buffers)
  (setq iflipb-saved-buffers nil)
  (let* ((buffers (iflipb-interesting-buffers))
         (current-buffer (nth index buffers)))
    (setq iflipb-current-buffer-index index)
    (setq iflipb-saved-buffers (iflipb-first-n index buffers))
    (message (iflipb-format-buffers current-buffer buffers))
    (switch-to-buffer current-buffer)))

(defun iflipb-next-buffer ()
  "Flip to the next buffer in the buffer list. Consecutive invocations
switch to less recent buffers in the buffer list."
  (interactive)
  (when (iflipb-first-iflipb-buffer-switch-command)
    (setq iflipb-current-buffer-index 0)
    (setq iflipb-saved-buffers nil))
  (if (= iflipb-current-buffer-index
         (1- (length (iflipb-interesting-buffers))))
      (message "No more buffers.")
    (iflipb-select-buffer (1+ iflipb-current-buffer-index)))
  (setq last-command 'iflipb-next-buffer))

(defun iflipb-previous-buffer ()
  "Flip to the previous buffer in the buffer list. Consecutive
invocations switch to more recent buffers in the buffer list."
  (interactive)
  (when (iflipb-first-iflipb-buffer-switch-command)
    (setq iflipb-current-buffer-index 0)
    (setq iflipb-saved-buffers nil))
  (if (= iflipb-current-buffer-index 0)
      (message "You are already looking at the top buffer.")
    (iflipb-select-buffer (1- iflipb-current-buffer-index)))
  (setq last-command 'iflipb-previous-buffer))

(provide 'iflipb)
