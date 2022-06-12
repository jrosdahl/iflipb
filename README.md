iflipb
======

[![MELPA](https://melpa.org/packages/iflipb-badge.svg)](https://melpa.org/#/iflipb)

iflipb lets you flip between recently visited buffers in a way that resembles
what _Alt-(Shift-)TAB_ does in Microsoft Windows and other graphical window
managers.


Operation
---------

iflipb provides three commands: `iflipb-next-buffer`, `iflipb-previous-buffer`
and `iflipb-kill-buffer`.

`iflipb-next-buffer` behaves like Alt-TAB: it switches to the previously used
buffer, just like _C-x b RET_ (or _C-M-l_ in XEmacs). However, another
consecutive call to `iflipb-next-buffer` switches to the next buffer in the
buffer list, and so on. When such a consecutive call is made, the skipped-over
buffer is not considered visited.

While flipping, the names of the most recent buffers are displayed in the
minibuffer, and the currently visited buffer is surrounded by square brackets
and marked with a bold face.

A key thing to notice here is that iflipb displays the buffer contents after
each step forward/backwards (in addition to displaying the buffer names),
unlike for instance the buffer switching model of `ido-mode` where only the
buffer names are displayed.

`iflipb-previous-buffer` behaves like `Alt-Shift-TAB`: it walks backwards in
the buffer list.

iflipb treats the buffer list as a stack, and (by design) it doesn't wrap
around. This means that when you have flipped to the last buffer and continue,
you don't get to the first buffer again. This is a good thing. (If you disagree
and want wrap-around, set `iflipb-wrap-around` to non-`nil`.)

Here is an illustration of what happens in a couple of different scenarios:

                       Minibuffer    Actual
                       display       buffer list
    --------------------------------------------
    Original:                        A B C D E
    Forward flip:      A [B] C D E   B A C D E
    Forward flip:      A B [C] D E   C A B D E
    Forward flip:      A B C [D] E   D A B C E

    Original:                        A B C D E
    Forward flip:      A [B] C D E   B A C D E
    Forward flip:      A B [C] D E   C A B D E
    Backward flip:     A [B] C D E   B A C D E

    Original:                        A B C D E
    Forward flip:      A [B] C D E   B A C D E
    Forward flip:      A B [C] D E   C A B D E
    [Edit buffer C]:                 C A B D E
    Forward flip:      C [A] B D E   A C B D E

iflipb by default ignores buffers whose name starts with an asterisk or space.
You can give a prefix argument to `iflipb-next-buffer` to make it flip between
more buffers. See the documentation of `iflipb-ignore-buffers` and
`iflipb-always-ignore-buffers` for how to change this.

`iflipb-kill-buffer` is designed to be bound to `C-x k`. It behaves like
`kill-buffer` but keeps iflipb's buffer list state so that it's possible to
kill a buffer and then advance to the next buffer without starting all over
again.


Installation
------------

To load iflipb, store `iflipb.el` in your Emacs load path and put

    (require 'iflipb)

in your Emacs configuration file.

iflipb does not install any key bindings for the two commands. I personally use
_M-h_ and _M-H_ (i.e., _M-S-h_) since I don't use the standard binding of _M-h_
(mark-paragraph) and _M-h_ is quick and easy to press. To install iflipb with
_M-h_ and _M-H_ as keyboard bindings:

    (global-set-key (kbd "M-h") 'iflipb-next-buffer)
    (global-set-key (kbd "M-H") 'iflipb-previous-buffer)

Another alternative is to use `C-tab` and `C-S-tab`:

    (global-set-key (kbd "<C-tab>") 'iflipb-next-buffer)
    (global-set-key
     (if (featurep 'xemacs) (kbd "<C-iso-left-tab>") (kbd "<C-S-iso-lefttab>"))
     'iflipb-previous-buffer)

Or perhaps use functions keys like `F9` and `F10`:

    (global-set-key (kbd "<f10>") 'iflipb-next-buffer)
    (global-set-key (kbd "<f9>")  'iflipb-previous-buffer)

Optionally bind `iflip-kill-buffer`:

    (global-set-key (kbd "C-x k") 'iflipb-kill-buffer)


Configuration
-------------

These variables affect iflipb's behavior:

* `iflipb-ignore-buffers` (default: `"^[*]"`)

  This variable determines which buffers to ignore when a prefix argument has
  not been given to `iflipb-next-buffer`. The value should be a regexp string, a
  function or a list. If the value is a regexp string, it describes buffer names
  to exclude from the buffer list. If the value is a function, the function will
  get a buffer name as an argument and should return `nil` if the buffer should
  be excluded, otherwise non-`nil`. If the value is a list, the filter matches
  if any of the list elements match.

* `iflipb-always-ignore-buffers` (default: `"^ "`)

  This variable determines which buffers to always ignore. The value should be a
  regexp string, a function or a list. If the value is a regexp string, it
  describes buffer names to exclude from the buffer list. If the value is a
  function, the function will get a buffer name as an argument and should return
  `nil` if the buffer should be excluded, otherwise non-`nil`. If the value is a
  list, the filter matches if any of the list elements match.

* `iflipb-wrap-around` (default: `nil`)

  This variable determines whether buffer cycling should wrap around when an
  edge is reached in the buffer list.

* `iflipb-permissive-flip-back` (default: `nil`)

  This variable determines whether `iflipb-previous-buffer` should use the
  previous buffer list when it's the first `iflipb-*-buffer` command in a row.
  In other words: Running `iflipb-previous-buffer` after editing a buffer will
  act as if the current buffer was not visited; it will stay in its original
  place in the buffer list.

* `iflipb-other-buffer-template` (default `"%s"`)

  This template string says how to display a non-current buffer name. `%s`
  expands to the buffer name.

* `iflipb-current-buffer-template` (default `"[%s]"`)

  This template string says how to display the current buffer name. `%s` expands
  to the buffer name.

* `iflipb-buffer-list-function` (default `#'iflipb-buffer-list`)

  This function is used to retrieve the buffer list. The current options are
  `'iflipb-buffer-list` and `'iflipb-ido-buffer-list`.

* `iflipb-format-buffers-function` (default
  `#'iflipb-format-buffers-horizontally`)

  This function is used to format buffer names. The function will get the
  current buffer and a buffer list as arguments. A return value is a string to
  be displayed. Predefined functions are `iflipb-format-buffers-horizontally`
  for a horizontal list and `iflipb-format-buffers-vertically` for a vertical
  list. See also `iflipb-format-buffers-height`.

* `iflipb-format-buffers-height` (default `5`)

  This variable determines the minibuffer height to use when displaying buffer
  names with `iflipb-format-buffers-vertically`. The actual height will not
  exceed the height indicated by `max-mini-window-height`.


About
-----

iflipb was inspired by
[cycle-buffer.el](https://www.emacswiki.org/emacs/cycle-buffer.el).
`cycle-buffer.el` has some more features, but doesn't quite behave like I want,
so I wrote my own simple replacement.

Have fun!

/Joel Rosdahl <joel@rosdahl.net>
