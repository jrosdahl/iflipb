;;; icycleb --- cycle between recently visited buffers

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
;; This file does not install any key bindings for the two commands. I
;; personally use M-h and M-H (i.e., M-S-h) since I don't use the
;; standard binding of M-h (mark-paragraph). To install icycleb with
;; M-h and M-H as keyboard bindings, put something like this in your
;; .emacs:
;;
;;     (load "icycleb")
;;     (global-set-key (kbd "M-h") 'icycleb-next-buffer)
;;     (global-set-key (kbd "M-H") 'icycleb-previous-buffer)
;;
;; icycleb was inspired by cycle-buffer.el
;; <http://kellyfelkins.org/pub/cycle-buffer.el>. cycle-buffer.el has
;; some more features, but doesn't quite behave like I want, so I
;; wrote my own simple replacement.
;;
;; /Joel Rosdahl <joel@rosdahl.net>

(defvar icycleb-current-buffer-index 0)
(defvar icycleb-saved-buffers nil)

(defun icycleb-first-n (n list)
  (if (= n 0)
      nil
    (cons (car list) (icycleb-first-n (1- n) (cdr list)))))

(defun icycleb-filter (elements fn)
  (if (null elements)
      nil
    (if (apply fn (list (car elements)))
        (cons (car elements)
              (icycleb-filter (cdr elements) fn))
      (icycleb-filter (cdr elements) fn))))

(defun icycleb-interesting-buffer-p (buffer)
  (not (eq (string-to-char (buffer-name buffer)) ?\ )))

(defun icycleb-interesting-buffers ()
  (icycleb-filter (buffer-list) 'icycleb-interesting-buffer-p))

(defun icycleb-first-buffer-command ()
  (not (or (eq last-command 'icycleb-next-buffer)
           (eq last-command 'icycleb-previous-buffer))))

(defun icycleb-restore-buffers (buffers)
  (when buffers
    (icycleb-restore-buffers (cdr buffers))
    (switch-to-buffer (car buffers))))

(defun icycleb-format-buffer (current-buffer buffer)
  (let ((name (buffer-name buffer)))
    (when (eq current-buffer buffer)
      (setq name (format "[%s]" name))
      (add-text-properties 1 (1- (length name)) '(face bold) name))
    name))
(defun icycleb-format-buffers (current-buffer buffers)
  (truncate-string-to-width
   (mapconcat
    (lambda (buffer)
      (icycleb-format-buffer current-buffer buffer))
    buffers
    " ")
   (1- (window-width (minibuffer-window)))))

(defun icycleb-select-buffer (index)
  (icycleb-restore-buffers icycleb-saved-buffers)
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
  (when (icycleb-first-buffer-command)
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
  (when (icycleb-first-buffer-command)
    (setq icycleb-current-buffer-index 0)
    (setq icycleb-saved-buffers nil))
  (if (= icycleb-current-buffer-index 0)
      (message "You are already looking at the top buffer.")
    (icycleb-select-buffer (1- icycleb-current-buffer-index)))
  (setq last-command 'icycleb-previous-buffer))
