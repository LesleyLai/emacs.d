Move Lines
==========

Emacs package that moves current line or lines surrounding region up or down.

![Demonstration](move-lines.gif)

Installation
------------

Copy the provided version of `move-lines.el` in a directory which is in the
Emacs `load-path`. Then, execute the following code either directly or in
your .emacs file:

```lisp
(require 'move-lines)
(move-lines-binding)
```

After that, you can move the line(s) up by **M-p** or **M-\<up\>** or down by
**M-n** or **M-\<down\>**.
