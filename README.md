# unmodified-buffer1.el

A minor mode that automatically restores an Emacs buffer modified state in case its contents match the not modified buffer content.

Also allow to prevent ```undo``` to jump to visible lines.

This package provides an Emacs hook to automatically revert a buffer's modified state, in case the buffer has been changed back to match the original content of the buffer.

Unlike "arthurcgusmao/unmodified-buffer" 2022 package, we don't use timer 0.5 sec, we don't save buffer to a file.

Instead we keep copy of buffer content in a string variable and check for modifications instanlty.

We copy buffer to a string at `before-change-functions' and at `before-change-functions'.

To detect that buffer returned to a first state, we do 4 steps:
1) compare buffer content by length to unmodified state.
2) compare current line with line in unmodified buffer.
3) compare all hashes of lines - it is 1) step stored as cache
4) compare full buffer to a string if 1), 2) and 3) was successful.

So, we never restore buffer without full comparision with saved copy.

## Configuration:
```elisp
(add-to-list 'load-path "/path/to/unmodified-buffer1/")
(require 'unmodified-buffer1)
(add-hook 'text-mode-hook 'unmodified-buffer1-mode) ;; Optional
(add-hook 'prog-mode-hook 'unmodified-buffer1-mode) ;; Optional
```

## Installation using straight.el and use-package:
```elisp
(use-package unmodified-buffer
   :straight (:host github :repo "Anoncheg1/emacs-unmodified-buffer1")
   :hook (after-init . unmodified-buffer1-global-mode)) ;; Optional
```

## Other packages
- Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
- Search with Chinese	https://github.com/Anoncheg1/pinyin-isearch
- Ediff fix		https://github.com/Anoncheg1/ediffnw
- Dired history	https://github.com/Anoncheg1/dired-hist
- Selected window contrast	https://github.com/Anoncheg1/selected-window-contrast
- Copy link to clipboard	https://github.com/Anoncheg1/emacs-org-links
- Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
- Call LLMs and AI agents from Org-mode ai block. https://github.com/Anoncheg1/emacs-oai
- Call LLMs and AI agents from Org-mode ai block. https://github.com/Anoncheg1/emacs-oai

## Donate, sponsor author
You can sponsor author crypto money directly with crypto currencies:
- BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
- USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
- TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D
