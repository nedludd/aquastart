;; A Railscasts theme for emacs 24
;;
;; This theme is based on the Textmate theme used by Ryan Bates in his
;; Railscasts tutorial.  The original theme is here: https://github.com/ryanb/textmate-themes.git
;;
;; Textmate defines the following colors. I've tried to translate them
;; to sensible emacs faces:
;;
;;    #E6E1DC Foreground
;;    #2B2B2B Background
;;
;;    #5A647E Selection
;;    #555577 Secondary Selection
;;
;;    #404040 Invisibles
;;
;;    #333435 Line Highlight
;;
;;    #232323 Source
;;
;;    #BC9458 Comment
;;    #CC7833 Keyword
;;    #FFC66D Function
;;    #A5C261 Number, String
;;    #D0D0FF Variable
;;    #6D9CBE Constant
;;    #DA4939 Constant (other)
;;    #E8BF6A Markup
;;
;;    #990000 Invalid (fg #FFFFFF)
;;    #519F50 String interpolation

;;    #144212 Diff Add (fg #E6E1DC)
;;    #660000 Diff Remove (fg #E6E1DC)
;;    #2F33AB Diff Header
;;
;; To use in emacs 24 (and only in emacs 24+) put this file in the
;; directory pointed to by 'color-theme-directory'.  Make sure the
;; name ends in "-theme.el".  Then do "M-x load-theme<RET>railscasts"
;; or put it in your 'init.el' as follows:
;;
;;      (load-theme 'railscasts)
;;
;;-----------------------------------------------------------------------


(deftheme railscasts
  "Railscasts theme based on Textmate theme of the same name")

(custom-theme-set-variables
 'railscasts)

(custom-theme-set-faces
 'railscasts

 ;; Standard Faces
 '(default                        ((((class color) (min-colors 89)) (:background "#2B2B2B" :foreground "#E6E1DC"))))
 '(cursor                         ((((class color) (min-colors 89)) (:foreground "#FFA500"))))
 '(mouse                          ((((class color) (min-colors 89)) (:foreground "#FFA500"))))
 '(escape-glyph                   ((((class color) (min-colors 89)) (:foreground "#DDAA6F"))))
 '(minibuffer-prompt              ((((class color) (min-colors 89)) (:foreground "#FFA500"))))
 '(highlight                      ((((class color) (min-colors 89)) (:background "#333435"))))
 '(region                         ((((class color) (min-colors 89)) (:background "#5A647E"))))
 '(secondary-selection            ((((class color) (min-colors 89)) (:background "#555577"))))
 ;; '(button                         ((((class color) (min-colors 89)) (:background "#333333" :foreground "#f6f3e8"))))
 '(isearch                        ((((class color) (min-colors 89)) (:background "#404040"))))
 '(query-replace                  ((((class color) (min-colors 89)) (:background "#404040"))))
 '(lazy-highlight                 ((((class color) (min-colors 89)) (:background "#404040"))))
 '(fringe                         ((((class color) (min-colors 89)) (:background "#232323"))))
 '(header-line                    ((((class color) (min-colors 89)) (:background "#404040" :foreground "#e7f6da"))))

 ;; Font lock
 '(font-lock-builtin-face         ((((class color) (min-colors 89)) (:foreground "#D0D0FF"))))
 '(font-lock-comment-face         ((((class color) (min-colors 89)) (:foreground "#BC9458" :italic t))))
 '(font-lock-constant-face        ((((class color) (min-colors 89)) (:foreground "#6D9CBE"))))
 '(font-lock-function-name-face   ((((class color) (min-colors 89)) (:foreground "#FFC66D"))))
 '(font-lock-keyword-face         ((((class color) (min-colors 89)) (:foreground "#CC7833"))))
 '(font-lock-string-face          ((((class color) (min-colors 89)) (:foreground "#A5C261"))))
 '(font-lock-type-face            ((((class color) (min-colors 89)) (:foreground "#FFFFFF"))))
 '(font-lock-variable-name-face   ((((class color) (min-colors 89)) (:foreground "#B0C4DE"))))
 '(font-lock-warning-face         ((((class color) (min-colors 89)) (:foreground "#EE799F" :weight normal))))

 ;; Ido
 '(ido-subdir                     ((((class color) (min-colors 89)) (:foreground "#CF6A4C"))))
 '(ido-first-match                ((((class color) (min-colors 89)) (:foreground "#8F9D6A"))))
 '(ido-only-match                 ((((class color) (min-colors 89)) (:foreground "#8F9D6A"))))

 ;; Parens
 '(paren-match-face               ((((class color) (min-colors 89)) (:background "#555577" :foreground "#FFC66D"))))
 '(paren-face-match               ((((class color) (min-colors 89)) (:background "#404040"))))
 '(show-paren-match-face          ((((class color) (min-colors 89)) (:background "#6D9CBE"))))
 '(show-paren-mismatch-face       ((((class color) (min-colors 89)) (:background "#990000"))))

 ;; Modeline
 '(mode-line                      ((((class color) (min-colors 89)) (:background "#A5BAF1" :foreground "#000000"))))
 '(mode-line-buffer-id            ((((class color) (min-colors 89)) (:background "#A5BAF1" :foreground "#000000"))))
 '(mode-line-mouseable            ((((class color) (min-colors 89)) (:background "#A5BAF1" :foreground "#000000"))))
 '(mode-line-mouseable-minor-mode ((((class color) (min-colors 89)) (:background "#A5BAF1" :foreground "#000000"))))
 ;; '(mode-line-inactive             ((((class color) (min-colors 89)) (:background "#6D9CBE" :foreground "#857b6f"))))

 ;; Perl
 '(cperl-array-face               ((((class color) (min-colors 89)) (:foreground "#6A5ACD"))))
 '(cperl-hash-face                ((((class color) (min-colors 89)) (:foreground "#FF4500"))))

 ;; Misc
 '(highline-face                  ((((class color) (min-colors 89)) (:background "#404040"))))
 '(link                           ((((class color) (min-colors 89)) (:foreground "#8ac6f2"))))
 '(link-visited                   ((((class color) (min-colors 89)) (:foreground "#e5786d"))))
 '(flymake-errline                ((((class color) (min-colors 89)) (:background "#FFA07A" :foreground "#000000"))))
 '(flymake-warnline               ((((class color) (min-colors 89)) (:background "#A5BAF1" :foreground "#000000"))))

 ;; Whitespace
 '(whitespace-space               ((((class color) (min-colors 89)) (:background "#2B2B2B" :foreground "#555"))))
 '(whitespace-line                ((((class color) (min-colors 89)) (:background "#2B2B2B" :foreground "darkgrey"))))
 '(whitespace-newline             ((((class color) (min-colors 89)) (:background "#2B2B2B" :foreground "#555"))))
 '(whitespace-space-after-tab     ((((class color) (min-colors 89)) (:background "#2B2B2B" :foreground "#DA4939"))))
 '(whitespace-empty               ((((class color) (min-colors 89)) (:background "#2B2B2B" :foreground "#CC9393"))))
 )

(provide-theme 'railscasts)
