;;; init.el --- My GNU Emacs Configuration

;; Copyright (C) 2020 Yogesh Kamat

;; Author: Yogesh Kamat
;; Keywords: Convenience
;; URL: https://github.com/yogeshk4/emacs.d

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains all the tweaks that I like to use with GNU Emacs.

;;; Code:

(require 'package)

;; MELPA
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives
	       (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.
  ;; See `package-archive-priorities` and `package-pinned-packages`.
  ;; Most users will not need or want to do this.
  ;; (add-to-list 'package-archives
  ;; 	       (cons "melpa-stable"
  ;; 		     (concat proto "://stable.melpa.org/packages/")) t)
  )

;; Org ELPA
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(package-initialize)

;; use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; theme
(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

;; don't show
(setq inhibit-startup-screen t)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; modeline
(column-number-mode 1)
(size-indication-mode 1)
(display-time-mode 1)
(use-package delight)
(use-package eldoc
  :delight)

;; relative live numbers
(setq-default display-line-numbers 'relative)

;; highlight current line
(global-hl-line-mode 1)

;; disable bell ring
(setq ring-bell-function 'ignore)

;; enable delete selection mode
(delete-selection-mode 1)

;; show matching parens
(show-paren-mode 1)

;; indentation can not insert tabs
(setq-default indent-tabs-mode nil)

;; automatic line-wrapping
(setq-default fill-column 80)

;; remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; shorter y/n answers
(defalias 'yes-or-no-p 'y-or-n-p)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; save customizations in a separate file
(setq custom-file "~/.emacs.d/emacs-custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; store backup and auto-save files to the systems temp directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; Packages:

(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package try)

(use-package which-key
  :delight
  :config
  (which-key-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package avy
  :bind (("C-:" . avy-goto-char-timer)
         ("C-c C-:" . avy-goto-line)))

(use-package ace-window
  :delight
  :bind ("M-o" . ace-window))

(use-package ivy
  :delight
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package swiper
  :bind ("C-s" . swiper-isearch))

(use-package counsel
  :delight
  :config
  (counsel-mode 1))

(use-package neotree
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-theme 'ascii))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package projectile
  :delight '(:eval (concat " Proj[" (projectile-project-name) "]"))
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode 1))

(use-package company
  :delight
  :hook (after-init . global-company-mode))

(use-package company-quickhelp
  :after company
  :bind (:map company-active-map
              ("C-c h" . company-quickhelp-manual-begin)))

;;; init.el ends here
