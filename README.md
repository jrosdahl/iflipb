iflipb
======

iflipb lets you flip between recently visited buffers in a way that resembles
what _Alt-(Shift-)TAB_ does in Microsoft Windows and other graphical window
managers. iflipb treats the buffer list as a stack, and (by design) it doesn't
wrap around. This means that when you have flipped to the last buffer and
continue, you don't get to the first buffer again. This is a good thing. (If
you disagree and want wrap-around, set `iflipb-wrap-around` to non-nil.)


Operation
---------

iflipb provides two commands: `iflipb-next-buffer` and
`iflipb-previous-buffer`.

`iflipb-next-buffer` behaves like Alt-TAB: it switches to the previously used
buffer, just like _C-x b RET_ (or _C-M-l_ in XEmacs). However, another
consecutive call to `iflipb-next-buffer` switches to the next buffer in the
buffer list, and so on. When such a consecutive call is made, the skipped-over
buffer is not regarded as visited.

While flipping, the names of the most recent buffers are displayed in the
minibuffer, and the currently visited buffer is surrounded by square brackets
and marked with a bold face.

A key thing to notice here is that iflipb displays the buffer contents after
each step forward/backwards (in addition to displaying the buffer names),
unlike for instance the buffer switching model of ido-mode where only the
buffer names are displayed.

`iflipb-previous-buffer` behaves like `Alt-Shift-TAB`: it walks backwards in
the buffer list.

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

iflipb by default ignores buffers whose names start with an asterisk or space.
You can give a prefix argument to `iflipb-next-buffer` to make it flip between
more buffers. See the documentation of the variables `iflipb-ignore-buffers`
and `iflipb-always-ignore-buffers` for how to change this.


Installation
------------

To load iflipb, store `iflipb.el` in your Emacs load path and put

    (require 'iflipb)

in your `.emacs` file or equivalent.

iflipb does not install any key bindings for the two commands. I personally use
_M-h_ and _M-H_ (i.e., _M-S-h_) since I don't use the standard binding of _M-h_
(mark-paragraph) and _M-h_ is quick and easy to press. To install iflipb with
_M-h_ and _M-H_ as keyboard bindings, put something like this in your `.emacs`:

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


About
-----

iflipb was inspired by
[cycle-buffer.el](http://kellyfelkins.org/pub/cycle-buffer.el).
`cycle-buffer.el` has some more features, but doesn't quite behave like I want,
so I wrote my own simple replacement.

Have fun!

/Joel Rosdahl <joel@rosdahl.net>
