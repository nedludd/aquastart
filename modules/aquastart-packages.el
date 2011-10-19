;; aquastart-packages - install additional libraries

(require 'package)

;; Use the Marmalade package repo
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Load up any packages not already installed
(when (not package-archive-contents)
  (package-refresh-contents))

;; These are the packages we want to start with
(defvar prelude-packages
  '(marmalade gist)
  "The list of packages that should be installed at launch.")

;; Install em now!
(dolist (p prelude-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Get the Marmalade uploads working
(setq marmalade-server "http://marmalade-repo.org")
;;(b 'marmalade)

(message "Loaded aquastart-packages")

(provide 'aquastart-packages)
