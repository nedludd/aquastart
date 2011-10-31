;; aquastart-ui -- UI configuration

;; customization
(defgroup ui nil
  "Aquastart UI"
  :group 'aquastart)

(defcustom aquastart-use-minimalistic-ui t
  "If set to true Aquastart will dispense of most the UI that's mouse related - 
menu bar, tool bar, etc"
  :type 'boolean
  :group 'ui)

(defcustom aquastart-use-smooth-scrolling t
  "Overrides the default scrolling behavior with a much more common one."
  :type 'boolean
  :group 'ui)

(defcustom aquastart-use-default-aquastart-theme t
  "If set to true Aquastart will load up its default theme (Railscasts),
instead of Emacs's default theme."
  :type 'boolean
  :group 'ui)

(defcustom aquastart-enhance-modeline t
  "If set to true Aquastart will augment the default modeline settings."
  :type 'boolean
  :group 'ui)


(when aquastart-use-minimalistic-ui
  ;; the toolbar is just a waste of valuable screen estate
  (tool-bar-mode -1)
  ;; Who needs scroll bars?
  (scroll-bar-mode -1)
  ;; the blinking cursor is nothing, but an annoyance
  (blink-cursor-mode -1)
  ;; disable the tab bar
  (tabbar-mode -1)
  ;; disable startup screen
  (setq inhibit-startup-screen t))

(when aquastart-use-smooth-scrolling
  ;; nice scrolling
  (setq scroll-margin 0
	scroll-conservatively 100000
	scroll-preserve-screen-position 1))

(when aquastart-enhance-modeline
  ;; mode line settings
  (line-number-mode t)
  (column-number-mode t)
  )

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat aquastart-dir "themes/"))

(when aquastart-use-default-aquastart-theme
  (load-theme 'railscasts t))

(message "Loaded aquastart-ui")

(provide 'aquastart-ui)
