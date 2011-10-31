;;; aquastart-base.el -- base Aquastart defuns
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Aquastart
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Aquastart.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

(require 'cl)
(require 'thingatpt)
(require 'imenu)

;; customization
(defgroup core nil
  "Emacs Aquastart core"
  :group 'aquastart)

(defun aquastart-add-subfolders-to-load-path (parent-dir)
  "Adds all first level `parent-dir' subdirs to the
Emacs load path."
  (dolist (f (directory-files parent-dir))
    (let ((name (concat parent-dir f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

;; add the first level subfolders of vendor automatically
(aquastart-add-subfolders-to-load-path aquastart-vendor-dir)

;;-------------------------------------------------------------------------

(ido-mode t)
(ido-ubiquitous t)
;; Put "." as the first item in a list of filenames, so you can easily
;; start dired in the current directory
(setq ido-show-dot-for-dired t)

;; Pull in recent files

; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

;; Use ido for recentf
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Open recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;; more useful.
(global-set-key (kbd "C-x f") 'ido-recentf-open)

;;-------------------------------------------------------------------------

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(setq smex-history-length 20) ;; Save 20 items of history
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;;-------------------------------------------------------------------------

;; Run Dired in a single buffer
(require 'dired-single)
(require 'dired-x)

(defun my-dired-init ()
  "Bunch of stuff to run for dired, either immediately or when it's loaded."
  ;; <add other stuff here>
  (define-key dired-mode-map [return] 'dired-single-buffer)
  (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
  (define-key dired-mode-map "^"
    (function
     (lambda nil (interactive) (dired-single-buffer ".."))))
  (lambda () (dired-omit-mode 1)))

;; if dired's already loaded, then the keymap will be bound
(if (boundp 'dired-mode-map)
    ;; we're good to go; just add our bindings
    (my-dired-init)
  ;; it's not loaded yet, so add our bindings to the load-hook
  (add-hook 'dired-load-hook 'my-dired-init))

;; Ask for a directory to dired
(global-set-key [(f5)] 'dired-single-magic-buffer)

;; Add a sort menu for dired
(add-hook 'dired-load-hook
    (lambda () (require 'dired-sort-menu)))

(add-hook 'dired-mode-hook
    (function (lambda ()
                  ;; Omit uninteresting files ("..", "*.elc", etc.)
                  (dired-omit-mode 1)
                  )))

;;-------------------------------------------------------------------------

;; iBuffer
(autoload 'ibuffer "ibuffer" "List buffers." t)
(setq ibuffer-sorting-mode 'recency)
(setq ibuffer-show-empty-filter-groups nil)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired   " (mode . dired-mode))
               ("shell   " (or
                            (mode . eshell-mode)
                            (mode . shell-mode)))
               ("music   " (or
                            (name . "^\\*EMMS")
                            (mode . emms-mode)))
               ("perl    " (mode . cperl-mode))
               ("ruby    " (or
                            (filename . "erb$")
                            (filename . "rhtml$")
                            (mode . ruby-mode)
                            (mode . yaml-mode)
                            (mode . inferior-ruby-mode)
                            (name . "^\\*rails\\*$")
                            (name . "^\\*RServer\\*$")))
               ("erc     " (mode . erc-mode))
               ("man     " (or
                            (mode . woman-mode)
                            (mode . man-mode)))
               ("organize" (or
                            (name . "^\\*Calendar\\*$")
                            (name . "^diary$")
                            (mode . muse-mode)
                            (mode . org-mode)
                            (mode . remember-mode)))
               ("web     " (or
                            (mode . js2-mode)
                            (mode . javascript-mode)
                            (mode . espresso-mode)
                            (filename . "js$")
                            (mode . haml-mode)
                            (mode . nxhtml-mode)
                            (mode . sass-mode)
                            (mode . scss-mode)
                            (mode . css-mode)))
               ("emacs   " (or
                            (mode . emacs-lisp-mode)
                            (name . "^\\*scratch\\*$")
                            (name . "^\\*Messages\\*$")))))))


(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))



(provide 'aquastart-base)
;;; aquastart-base.el ends here
