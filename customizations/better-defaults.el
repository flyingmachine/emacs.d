;;; better-defaults.el --- Fixing weird quirks and poor defaults

;; Copyright © 2013 Phil Hagelberg

;; Author: Phil Hagelberg
;; URL: https://github.com/technomancy/better-defaults
;; Version: 0.1.2
;; Created: 2013-04-16
;; Keywords: convenience

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; There are a number of unfortunate facts about the way Emacs works
;; out of the box. While all users will eventually need to learn their
;; way around in order to customize it to their particular tastes,
;; this package attempts to address the most obvious of deficiencies
;; in uncontroversial ways that nearly everyone can agree upon.

;; Obviously there are many further tweaks you could do to improve
;; Emacs, (like those the Starter Kit and similar packages) but this
;; package focuses only on those that have near-universal appeal.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;;###autoload
(progn

  ;; ido-mode allows you to more easily navigate choices. For example,
  ;; when you want to switch buffers, ido presents you with a list
  ;; of buffers in the the mini-buffer. As you start to type a buffer's
  ;; name, ido will narrow down the list of buffers to match the text
  ;; you've typed in
  ;; http://www.emacswiki.org/emacs/InteractivelyDoThings
  (ido-mode t)
  
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  (setq ido-enable-flex-matching t)

  ;; Turn this behavior off because it's annoying
  (setq ido-use-filename-at-point nil)
  
  ;; This enables ido in all contexts where it could be useful, not just
  ;; for selecting buffer and file names
  (ido-ubiquitous-mode 1)

  ;; Turn off the menu icons at the top of the screen because they're buggy
  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  ;; Don't show native OS scroll bars for buffers because they're redundant
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;; "When several buffers visit identically-named files,
  ;; Emacs must give the buffers distinct names. The usual method
  ;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
  ;; of the buffer names (all but one of them).
  ;; The forward naming method includes part of the file's directory
  ;; name at the beginning of the buffer name
  ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  ;; When you visit a file, point goes to the last place where it
  ;; was when you previously visited the same file.
  ;; http://www.emacswiki.org/emacs/SavePlace
  (require 'saveplace)
  (setq-default save-place t)
  
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "C-x C-b") 'ibuffer)

  ;; Interactive search key bindings. By default, C-s runs
  ;; isearch-forward, so this swaps the bindings.
  (global-set-key (kbd "C-s") 'isearch-forward-regexp)
  (global-set-key (kbd "C-r") 'isearch-backward-regexp)
  (global-set-key (kbd "C-M-s") 'isearch-forward)
  (global-set-key (kbd "C-M-r") 'isearch-backward)

  ;; Enhances M-x to allow easier execution of commands. Provides
  ;; a filterable list of possible commands in the minibuffer
  ;; http://www.emacswiki.org/emacs/Smex
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)

  ;; Highlights matching parenthesis
  (show-paren-mode 1)
  (setq-default indent-tabs-mode nil)
  (setq x-select-enable-clipboard t
        x-select-enable-primary t
        save-interprogram-paste-before-kill t
        apropos-do-all t
        mouse-yank-at-point t
        save-place-file (concat user-emacs-directory "places")
        backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

(provide 'better-defaults)
;;; better-defaults.el ends here
