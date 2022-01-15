;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
;; Enable export in ODT format
;; (setcdr (assq 'system org-file-apps-defaults-gnu ) "xdg-open %s")
;; (setq process-connection-type nil)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name ""
      user-mail-address "")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-gruvbox)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/")

;; readno
     (setq org-todo-keywords
           '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(c)")
             (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)")
             ))
;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; it is not set to 'relative by default..
(setq display-line-numbers-type 'relative)



;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.



;; always enable rainbow parenthesis, textmode is default mode which everything is built on
;; (add-hook 'text-mode 'rainbow-delimiters-mode)

;; stick to 8 char tabs and indents, linux kernel style
;; https://www.kernel.org/doc/html/v4.10/process/coding-style.html
(setq-default c-basic-offset 8)


;; auto fill mode in org mode, as it can become too long if you're not hard wrapping
(add-hook 'org-mode-hook 'auto-fill-mode)


(setq org-agenda-files (list

                             "~/Documents/organisering/"
                             "~/sg/*"
                             )
      )



;; runs comman org-odt-convert-processes
;; use docx format instead of odt, as microsoft word says the file is dangerous if in .odt


;; use programs as default programs
;; it uses programs defined in "/etc/mailcap" or "$HOME/.mailcap"
;; update to default mimetypes use shell command "mime-update" did not work :(
(setq org-odt-styles-file "~/.config/emacs/stil-aflevering-style.ott")

;; colum size of 80 chars(the same as a terminal window)
(setq-default fill-column 80)

;; allow dead-acute á and other accenting to work
(require 'iso-transl)

;; vim visual line go down binding for orgmode as this is the vim way
;; and if you like having lines soft folded/wrapped then it is for you
;; (add-hook 'org-mode-hook '(lambda ()
;; (define-key (current-local-map) [(g-k)] (evil-previous-visual-line) )
;; (define-key (current-local-map) [(g-j)] (evil-next-visual-line) )
  ;; ))

;; Okular

(setq TeX-view-program-list '(("Okular" "okular --unique %u")))

(add-hook 'LaTeX-mode-hook '(lambda ()
                  (add-to-list 'TeX-expand-list
                       '("%u" Okular-make-url))))

(defun Okular-make-url () (concat
               "file://"
               (expand-file-name (funcall file (TeX-output-extension) t)
                         (file-name-directory (TeX-master-file)))
               "#src:"
               (TeX-current-line)
               (expand-file-name (TeX-master-directory))
               "./"
               (TeX-current-file-name-master-relative)))

(setq TeX-view-program-selection '((output-pdf "Okular")))


     (add-hook 'org-mode-hook
               (lambda ()
                 (define-key evil-normal-state-map (kbd "gk")
                   'evil-previous-visual-line)
                 (define-key evil-normal-state-map (kbd "gj")
                             'evil-next-visual-line)))
     ;; evil mode uses another table than evil....
