;;; unmodified-buffer1.el --- Better manage line numbers in your org-mode links -*- lexical-binding: t -*-

;; Author: <github.com/Anoncheg1,codeberg.org/Anoncheg>
;; Keywords: file, convenience
;; URL: https://github.com/Anoncheg1/emacs-unmodified-buffer1
;; Version: 0.1
;; Created: 13 Nov 2025
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (c) 2025 github.com/Anoncheg1,codeberg.org/Anoncheg

;;; License

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; Licensed under the GNU Affero General Public License, version 3 (AGPLv3)
;; <https://www.gnu.org/licenses/agpl-3.0.en.html>

;;; Commentary:
;; This package provides an Emacs hook to automatically revert a buffer's
;; modified state, in case the buffer has been changed back to match the
;; original content of the buffer.
;; Unlike "arthurcgusmao/unmodified-buffer" 2022 package,
;; we don't use timer 0.5 sec, we don't save buffer to a file.
;; Instead we keep copy of buffer content in a string variable and
;; check for modifications instanlty.

;; We copy buffer to a string at `before-change-functions' and at `before-change-functions'.
;;
;; To detect that buffer returned to a first state, we do 4 steps:
;; 1) compare buffer content by length to unmodified state.
;; 2) compare current line with line in unmodified buffer.
;; 3) compare all hashes of lines - it is 1) step stored as cache
;; 4) compare full buffer to a string if 1) and 2) was successful.

;; Configuration:
;; (add-to-list 'load-path "/path/to/unmodified-buffer1/")
;; (require 'unmodified-buffer1)
;; (add-hook 'text-mode-hook 'unmodified-buffer1-mode)
;; (add-hook 'prog-mode-hook 'unmodified-buffer1-mode)
;; Installation using straight.el and use-package:

;; (use-package unmodified-buffer
;;   :straight (:host github :repo "Anoncheg1/emacs-unmodified-buffer1")
;;   :hook (after-init . unmodified-buffer1-global-mode)) ;; Optional

;; For very special cases, if you have freezes, you may wrap some
;; functions (like prog-fill-reindent-defun and indent) with setting
;; inhibit-modification-hooks to nil or `combine-after-change-calls'
;; macro. See commented code at bottom of this file.

;; Touch: My body wants to be fried slowly. My mind wants to succeed.
;;   My soul don't need anything.

;; Other packages:
;; - Navigation in Dired, Packages, Buffers modes https://github.com/Anoncheg1/firstly-search
;; - Search with Chinese		https://github.com/Anoncheg1/pinyin-isearch
;; - Ediff no 3-th window		https://github.com/Anoncheg1/ediffnw
;; - Dired history			https://github.com/Anoncheg1/dired-hist
;; - Selected window contrast		https://github.com/Anoncheg1/selected-window-contrast
;; - Copy link to clipboard		https://github.com/Anoncheg1/org-links
;; - Solution for "callback hell"	https://github.com/Anoncheg1/emacs-async1
;; - outline.el usage			https://github.com/Anoncheg1/emacs-outline-it
;; - Call LLMs and AI agents from Org-mode ai block. https://github.com/Anoncheg1/emacs-oai

;; Donate:
;; - BTC (Bitcoin) address: 1CcDWSQ2vgqv5LxZuWaHGW52B9fkT5io25
;; - USDT (Tether) address: TVoXfYMkVYLnQZV3mGZ6GvmumuBfGsZzsN
;; - TON (Telegram) address: UQC8rjJFCHQkfdp7KmCkTZCb5dGzLFYe2TzsiZpfsnyTFt9D


;;; Code:

;;; - Variables

;;;###autoload
(defgroup unmodified-buffer1 nil
  "Automatically restore a buffer's modified state."
  :group 'tools)

(defun unmodified-buffer1--run-after-save-hook-function ()
  "Default hook that run `after-save-hook' without our.
Not require for mode, may be safely removded from
`unmodified-buffer1-hook'."
  (remove-hook 'after-save-hook #'unmodified-buffer1-save-or-revert t)
  (run-hooks 'after-save-hook)
  (add-hook 'after-save-hook #'unmodified-buffer1-save-or-revert nil t))

(defcustom unmodified-buffer1-hook '(unmodified-buffer1--run-after-save-hook-function)
  "Normal hook that is run after a buffer is set to unmodified."
  :type 'hook
  :group 'unmodified-buffer1)


(defvar-local unmodified-buffer1--unmod-content nil
  "Buffer content as it was last unmodified.")

(defvar-local unmodified-buffer1--unmod-content-length nil
  "Flag if content has been saved since last modification.
Also length of content string.")

;;; - String at pos

(defun unmodified-buffer1--str-line-at-pos (pos str)
  "Get line by position POS from string STR.
POS from 0.
Return (LINE START-POS), where LINE is the line in STR containing char
at POS, and START-POS is the index of the beginning of the line (first
char or boundary).
If POS at new line, return character before it.
If POS is at an empty line, returns (\"\" N), where N is the index after
preceding newline or 0."
  (unless str
    (error "unmodified-buffer1--str-line-at-pos: Str is nil"))
  (let ((len (length str)))
    (if (zerop len)
        '("" 0)
      (let* ((pos (max 0 (min pos (1- len))))
             (start (let ((s pos))
                      (while (and (> s 0)
                                  (not (char-equal (aref str (1- s)) ?\n)))
                        (setq s (1- s)))
                      s))
             (end (let ((e pos))
                    (while (and (< e len)
                                (not (char-equal (aref str e) ?\n)))
                      (setq e (1+ e)))
                    e)))
        (if (= start end)
            (list "" start)
          (list (substring str start end) start))))))

;; (unmodified-buffer1--str-line-at-pos 0 "a\nb\nc") ;; "a" 0
;; (unmodified-buffer1--str-line-at-pos 1 "a\nb\nc") ;; "a" 0
;; (unmodified-buffer1--str-line-at-pos 2 "a\nb\nc") ;; "b" 2
;; (unmodified-buffer1--str-line-at-pos 0 nil) ;; => error
;; (unmodified-buffer1--str-line-at-pos 0 "")      ;; => "" 0
;; (unmodified-buffer1--str-line-at-pos 3 "aaa\nbbb\n")                    ;; => "aaa" 0
;; (unmodified-buffer1--str-line-at-pos 4 "aaa\nbbb\n")                    ;; => "bbb" 4
;; (unmodified-buffer1--str-line-at-pos 4 "aaa\n\nbbb\n")                  ;; => "" 4
;; (unmodified-buffer1--str-line-at-pos 5 "aaa\nbbb\n")                    ;; => "bbb" 4
;; (unmodified-buffer1--str-line-at-pos 8 "aaa\nbbb\nccc")                 ;; => "ccc" 8
;; (unmodified-buffer1--str-line-at-pos 4 "aaa\nbbb\n")                    ;; => "bbb" 4
;; (unmodified-buffer1--str-line-at-pos 0 "aaa\nbbb\n")                    ;; => "aaa"
;; (unmodified-buffer1--str-line-at-pos 7 "aaa\nbbb\n")                    ;; => "bbb"
;; (unmodified-buffer1--str-line-at-pos 9 "aaa\nbbb\n")                    ;; => "bbb"
;; (unmodified-buffer1--str-line-at-pos 50 "aaa\nbbb\nccc")                ;; => "ccc" 8


;;; - Dict - hash table

(defvar unmodified-buffer1--dict (make-hash-table :test 'eq)
  "Cache dictionary for hashes of lines of unmodified buffer.
Key is a begin of line in nonmodified buffer; Value is integer hash of
unmodified buffer line.  `eq' is used to find position.")

(defun unmodified-buffer1--dict-compare (key value)
  "KEY is line begin position in buffer, VALUE is modified line.
Put line to dict of `unmodified-buffer1--unmod-content' if not cached.
Return
- nil - if not equal,
- t - if hash of value is equal to stored hash or line of stored buffer
  in `unmodified-buffer1--unmod-content'."
  ;; (print (list "unmodified-buffer1--dict-compare1" key value))
  (let* ((hash-value (sxhash-equal value))
         (found (gethash key unmodified-buffer1--dict))) ; hash of line
    (if found
        (eq found hash-value) ; compare
      ;; else - not found - put new and compare (puthash return value)
      (let ((res (unmodified-buffer1--str-line-at-pos (1- key) unmodified-buffer1--unmod-content)))
        ;; (print (list "unmodified-buffer1--dict-compare2" res))
        (eq (puthash (cadr res)
                     (sxhash-equal (car res)) ; value
                     unmodified-buffer1--dict)
            hash-value)))))

(defun unmodified-buffer1--all (l)
  "Like python all function applied to L list."
  (seq-every-p #'identity l))

(defun unmodified-buffer1--dict-compare-all ()
  "Compare saved hashes of lines that was modified with current buffer."
  (save-excursion
    (let (re) ; collecting results from maphash
      (maphash (lambda (k v)
                 "k is a line begin position (from 0), v is hash of line."
                 (goto-char (1+ k))
                 ;; (print (list (eq (sxhash-equal (buffer-substring-no-properties (1+ k) (line-end-position)))
                 ;;                  v)
                 ;;              (buffer-substring-no-properties (1+ k) (line-end-position))
                 ;;              v))
                 (push (eq (sxhash-equal (buffer-substring-no-properties (1+ k) (line-end-position)))
                           v)
                       re))
               unmodified-buffer1--dict)
      (unmodified-buffer1--all re))))

(defun unmodified-buffer1--dict-cl ()
  "Clear hash table from lines."
  (clrhash unmodified-buffer1--dict))

;; (puthash (sxhash-eq 1) (sxhash-equal "value") unmodified-buffer1--dict)

;; (unmodified-buffer1--dict-cl)
;; (unmodified-buffer1--dict-put 12 "some line") ; => hash
;; (unmodified-buffer1--dict-compare 12 "some line") ; => t
;; (gethash (sxhash-eq 13) unmodified-buffer1--dict) ; => nil
;; (unmodified-buffer1--dict-compare 13 "some line") ; => nil
;; (gethash (sxhash-eq 13) unmodified-buffer1--dict) ; hash of lines
;; (maphash (lambda (k v)
;;                  (print (list k v)
;;                  ))
;;                unmodified-buffer1--dict)
;; (unmodified-buffer1--dict-compare-all)
;; (defvar-local before-change-point-pos nil)

(defun unmodified-buffer1-save-unmodified-content (_beg _end)
  "Save buffer contents before the first modification.
Hook for `before-change-functions'."
  ;; (setq before-change-point-pos (point))
  ;; (print (list "unmodified-buffer1-save-unmodified-content" (not (buffer-modified-p)) (not unmodified-buffer1--unmod-content-length)))
  (when (and (not (buffer-modified-p)) ; not modified now
             (not unmodified-buffer1--unmod-content-length)) ; was not saved
    (save-restriction
      ;; (print (list "save"))
      (widen)
      (setq unmodified-buffer1--unmod-content (buffer-substring-no-properties (point-min) (point-max)))
      (setq unmodified-buffer1--unmod-content-length (buffer-size)))
    (unmodified-buffer1--dict-cl)))

(defun unmodified-buffer1-check-equal (_pbeg _pend _len)
  "Main function that check that buffer now is not modified.
Hook for `after-change-functions'."

  (when (and (buffer-modified-p)
             (not (buffer-narrowed-p))
             ;; 1)
             (eq unmodified-buffer1--unmod-content-length (buffer-size)))
    ;; (print (list "unmodified-buffer1-check-equal" (current-buffer) (buffer-modified-p) (eq unmodified-buffer1--unmod-content-length (buffer-size))))
    ;; (let* (
    ;;        ;; (backtrace-line-length 20) ; used by `backtrace-get-frames'
    ;;        ;; (print-level 10)
    ;;        ;; (print-length 19)
    ;;        (bt
    ;;         ;; (with-output-to-string (backtrace))
    ;;         (backtrace-to-string (backtrace-get-frames 'backtrace))))
    ;;   (print bt))
    (when (and
           ;; 2)
           (unmodified-buffer1--dict-compare (line-beginning-position)
                                             (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
           ;; 3) may fail at `buffer-substring-no-properties' with "args-out-of-range" error for Buffer List mode.
           (condition-case nil
               (unmodified-buffer1--dict-compare-all)
             (error nil))
           ;; 4)
           (string-equal unmodified-buffer1--unmod-content
                         (buffer-substring-no-properties (point-min) (point-max))))
      ;; (print "restore")
      (unmodified-buffer1--dict-cl)
      (set-buffer-modified-p nil)
      (when-let (bfn (buffer-file-name))
        (unlock-file bfn))
      (run-hooks 'unmodified-buffer1-hook))))

(defun unmodified-buffer1-save-or-revert ()
  "Re-save content after buffer is saved or reverted."
  (save-restriction
    ;; (print (list "save"))
    (widen)
    (setq unmodified-buffer1--unmod-content (buffer-substring-no-properties (point-min) (point-max)))
    (setq unmodified-buffer1--unmod-content-length (buffer-size)))
  (unmodified-buffer1--dict-cl))


(defun unmodified-buffer1-add-hooks ()
  "Setup hooks for this buffer for unmodified content capture."
  (add-hook 'before-change-functions	#'unmodified-buffer1-save-unmodified-content nil t) ; save
  (add-hook 'after-change-functions	#'unmodified-buffer1-check-equal nil t)
  (add-hook 'after-save-hook		#'unmodified-buffer1-save-or-revert nil t) ; save
  (add-hook 'after-revert-hook		#'unmodified-buffer1-save-or-revert nil t))

(defun unmodified-buffer1-remove-hooks ()
  "Remove hooks."
  (remove-hook 'before-change-functions	#'unmodified-buffer1-save-unmodified-content t)
  (remove-hook 'after-change-functions		#'unmodified-buffer1-check-equal t)
  (remove-hook 'after-save-hook		#'unmodified-buffer1-save-or-revert t)
  (remove-hook 'after-revert-hook		#'unmodified-buffer1-save-or-revert t))

;;; - Mode

;;;###autoload
(define-minor-mode unmodified-buffer1-mode
  "Automatically update a buffer's modified state.

Minor mode for automatically restoring a buffer state to
unmodified if its current content matches that of the file it
visits."
  :global nil
  (if unmodified-buffer1-mode
      (unmodified-buffer1-add-hooks)
    (unmodified-buffer1-remove-hooks)))

;;;###autoload
(define-globalized-minor-mode unmodified-buffer1-global-mode
  unmodified-buffer1-mode (lambda () (unmodified-buffer1-mode 1)))

(defun unmodified-buffer1-mode-disable-all ()
  "Disable unmodified-buffer in all existing buffers.

This is a utility function due to the way in which
`define-globalized-minor-mode' operates -- it doesn't turn the
mode off of existing buffers when deactivated."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (unmodified-buffer1-mode -1))))

;;; - faster
;; (defun unmodified-buffer1-inhibit-modification-hooks (func-call &rest args)
;;   (combine-after-change-calls
;;   ;; (let ((inhibit-modification-hooks t))
;;     (apply func-call args)))

;; (advice-add 'prog-fill-reindent-defun :around #'unmodified-buffer1-inhibit-modification-hooks)
;;; provide
(provide 'unmodified-buffer1)

;;; unmodified-buffer1.el ends here
