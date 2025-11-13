# unmodified-buffer1.el

A minor mode that automatically restores an Emacs buffer modified state in case its contents match the not modified buffer content.

This package provides an Emacs hook to automatically revert a buffer's modified state, in case the buffer has been changed back to match the original content of the buffer.

Unlike "arthurcgusmao/unmodified-buffer" 2022 package, we don't use timer 0.5 sec, we don't save buffer to a file.

Instead we keep copy of buffer content in a string variable and check for modifications instanlty.

We copy buffer to a string at `before-change-functions' and at `before-change-functions'.

To detect that buffer returned to a first state, we do 4 steps:
1) compare buffer content by length to unmodified state.
2) compare current line with line in unmodified buffer.
3) compare all hashes of lines - it is 1) step stored as cache
4) compare full buffer to a string if 1) and 2) was successful.

## Configuration:
```elisp
(add-to-list 'load-path "/path/to/unmodified-buffer1/")
(require 'unmodified-buffer1)
(add-hook 'after-init-hook 'unmodified-buffer1-global-mode) ;; Optional
```

## Installation using straight.el and use-package:
```elisp
(use-package unmodified-buffer
   :straight (:host github :repo "Anoncheg1/emacs-unmodified-buffer1")
   :hook (after-init . unmodified-buffer1-global-mode)) ;; Optional
```