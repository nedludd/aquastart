;; init.el -- Aquastart configuration entry point
;;
;; Author: Tim Hermans <edwardludd@gmail.com>
;; URL: http://github.com/nedludd/aquamacs-emacs-starter-kit
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules for Aquastart

;; IMPORTANT: make sure the code commented here is in
;; ~/Library/Aquamacs Emacs/Preferences.el or Aquastart won't load
;;; --- begin code for Preferences.el ------------------
;;;; Aquastart -- Aquamacs Emacs Starter Kit
;;;; http://github.com/nedudd/aquastart.git
;;
;;;; Set the base directory for Aquastart
;;(setq aquastart-dir (concat (file-name-directory
;;                    (or (buffer-file-name) load-file-name)
;;) "aquastart/"))
;;;; set up our various directories to load
;;(add-to-list 'load-path aquastart-dir)
;;(require 'init)
;;; --- end code for Preferences.el ----------------------

(defgroup aquastart nil
  "Aquastart"
  :group 'convenience)

;; Duh
(unless (string= system-type "darwin")
  (message "This starter kit is meant for Aquamacs on OS X"))

;; On OS X Emacs doesn't use the shell PATH if it's not started from
;; the shell. If you're using homebrew modifying the PATH is essential.
(push "/usr/local/bin" exec-path)

;; Directory settings
(defvar aquastart-dir (file-name-directory load-file-name)
   "The root dir of the Aquastart distribution.")
 (defvar aquastart-modules-dir (concat aquastart-dir "modules/")
   "The home of all the Aquastart modules.")
 (defvar aquastart-vendor-dir (concat aquastart-dir "vendor/")
   "This directory house Emacs Lisp packages that are not yet available in
 ELPA (or Marmalade).")
 (defvar aquastart-personal-dir (concat aquastart-dir user-login-name "/")
   "Home for all user specific libraries or configurations. All
 Emacs Lisp files there are loaded automatically by Aquastart.")
 (defvar aquastart-personal-file (concat aquastart-dir user-login-name ".el")
   "User configuration file.  Add your personal configurations here.")
(defvar aquastart-host-dir (concat aquastart-dir system-name)
  "Host specific configurations go in here.")

(add-to-list 'load-path aquastart-modules-dir)
(add-to-list 'load-path aquastart-vendor-dir)
(add-to-list 'load-path aquastart-personal-dir)

;; ;; the core stuff
(require 'aquastart-ui)
(require 'aquastart-packages)
(require 'aquastart-base)
(require 'aquastart-editor)
(require 'aquastart-global-keybindings)

;; ;; programming & markup languages support
;; (require 'aquastart-c)
;; (require 'aquastart-clojure)
;; (require 'aquastart-coffee)
;; (require 'aquastart-common-lisp)
;; (require 'aquastart-emacs-lisp)
;; (require 'aquastart-haskell)
;; (require 'aquastart-js)
;; (require 'aquastart-latex)
;; (require 'aquastart-markdown)
;; (require 'aquastart-perl)
;; (require 'aquastart-ruby)
;; (require 'aquastart-scheme)
;; (require 'aquastart-xml)

;; ;; config changes made through the customize UI will be store here
;; (setq custom-file (concat aquastart-personal-dir "custom.el"))
;; ;; load the personal settings
;; (when (file-exists-p aquastart-personal-dir)
;;   (mapc 'load (directory-files aquastart-personal-dir nil "^[^#].*el$")))

;; ;;; init.el ends here



;; (setq site-lisp-dir (concat kitfiles-dir "/site-lisp"))
;; (add-to-list 'load-path site-lisp-dir)

;; (setq settings-dir (concat "~/Library/Application Support/Aquamacs Emacs/"))

;; ;; Load package manager
;; (require 'package-manager-setup)

;; ;; general coding/editing niceties
;; (require 'whitespace)

;; ;;;;; Aquamacs Emacs Starter Kit specific customizations
;; (require 'adjust-path)
;; (require 'appearance)
;; (require 'anything-setup)
;; (require 'auto-complete-setup)
;; (require 'dired-setup)
;; (require 'ibuffer-setup)
;; (require 'ido-setup)
;; (require 'js2-mode-setup)
;; (require 'markdown-mode-setup)
;; (require 'org-mode-stuff)
;; (require 'override-aquamacs-to-be-more-emacsy)
;; (require 'prefer-utf)
;; (require 'smex-setup)
;; (require 'useful-functions)
;; (require 'yasnippet-setup)
;; ;; contains whitespace config for various modes and more
;; (require 'misc-mode-tweaks)

(message "Loaded Aquastart")

;; ;;;;; end Aquamacs Emacs Starter Kit specific customizations

(provide 'init)
