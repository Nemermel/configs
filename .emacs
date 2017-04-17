(require 'package) ;; You might already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Helm
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-s") 'helm-occur)
(setq helm-M-x-fuzzy-match t)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;(popwin-mode 1)
;(setq display-buffer-function 'popwin:display-buffer)
;(push '("^\*helm .+\*$" :regexp t) popwin:special-display-config)
;(push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)

;; Evil mode config
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 0)

;; Theme load
(load-theme 'base16-mocha t)
;;(setq badwolf-diff-hl-inverse t)

;; Flycheck


;; Ranger
(global-set-key [f8] 'ranger)
(setq ranger-cleanup-eagerly t)
(setq ranger-parent-depth 1)
;(push '("^\*ranger .+\*$" :regexp t) popwin:special-display-config)
;(push '("^\*ranger-.+\*$" :regexp t) popwin:special-display-config)

;; Line wrapping
(setq word-wrap          t) ;; переносить по словам
(global-visual-line-mode t)

;; Auto-Complete
(defun ac-init()
    (require 'auto-complete-config)
    (ac-config-default)
    (setq ac-auto-start        t)
    (setq ac-auto-show-menu    t)
    (global-auto-complete-mode t)
    (add-to-list 'ac-modes   'lisp-mode)
		(add-to-list 'ac-modes   'coffee-mode)
		(add-to-list 'ac-modes   'web-mode)
		(add-to-list 'ac-modes   'js2-mode)
    (add-to-list 'ac-sources 'ac-source-semantic) ;; искать автодополнения в CEDET
    (add-to-list 'ac-sources 'ac-source-variables) ;; среди переменных
    (add-to-list 'ac-sources 'ac-source-functions) ;; в названиях функций
    (add-to-list 'ac-sources 'ac-source-dictionary) ;; в той папке где редактируемый буфер
    (add-to-list 'ac-sources 'ac-source-words-in-all-buffer) ;; по всему буферу
    (add-to-list 'ac-sources 'ac-source-files-in-current-dir))

(ac-init)

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t) ;; экран приветствия можно вызвать комбинацией C-h C-a

; Show-paren-mode settings
(show-paren-mode t) ;; включить выделение выражений между {},[],()
(setq show-paren-style 'expression) ;; выделить цветом выражения между {},[],()

;; Electric-modes settings
(electric-pair-mode    1) ;; автозакрытие {},[],() с переводом курсора внутрь скобок
(electric-indent-mode -1) ;; отключить индентацию  electric-indent-mod'ом (default in Emacs-24.4)

;;Fringe
(fringe-mode 10)

;;Gitgutter
(require 'git-gutter-fringe+)

(git-gutter-fr+-minimal)
(set-face-foreground 'git-gutter+-modified "yellow")
(set-face-foreground 'git-gutter+-added    "green")
(set-face-foreground 'git-gutter+-deleted  "red")

;; Disable GUI components
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq use-dialog-box     nil)
(setq redisplay-dont-pause t)

;; Scrolling settings
(setq scroll-step 1)
(setq scroll-margin 10)
(setq scroll-conservatively 10000)

;; Indent tabs
;;(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default c-basic-offset 2)
(setq-default standard-indent 2)
(setq-default
 ;; js2-mode
 js2-basic-offset 2
 ;; web-mode
 css-indent-offset 2
 web-mode-markup-indent-offset 2
 web-mode-css-indent-offset 2
 web-mode-code-indent-offset 2
 web-mode-attr-indent-offset 2)

;; PHP Config
(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 2))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence my-tab-width 200 my-tab-width))))

(defun add-auto-mode (mode &rest patterns)
  (mapc (lambda (pattern)
          (add-to-list 'auto-mode-alist (cons pattern mode)))
        patterns))

(add-auto-mode 'web-mode
               "*html*" "*twig*" "*tmpl*" "\\.erb" "\\.rhtml$" "\\.ejs$" "\\.hbs$"
               "\\.ctp$" "\\.tpl$" "/\\(views\\|html\\|templates\\)/.*\\.php$")

;; Coffee/Javascript config
(add-to-list 'auto-mode-alist (cons (rx ".js" eos) 'js2-mode))
(add-hook 'js-mode-hook 'js2-minor-mode)

(add-hook 'js2-mode-hook 'my-disable-indent-tabs-mode)
(defun my-disable-indent-tabs-mode ()
  (set-variable 'indent-tabs-mode nil))

(defun coffee-custom ()
  "coffee-mode-hook"
  (make-local-variable 'tab-width)
  (set 'tab-width 2))

(add-hook 'coffee-mode-hook 'coffee-custom)

(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js2-mode-hook 'js-custom)


;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; Short messages in minibuffer
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clipboard settings
(setq x-select-enable-clipboard t)

;; Easy transition between buffers: M-<Arrow-Key>
(if (equal nil (equal major-mode 'org-mode))
    (windmove-default-keybindings 'meta))

;; Delete trailing whitespaces, format buffer and untabify when save buffer
(defun format-current-buffer()
    (indent-region (point-min) (point-max)))
(defun untabify-current-buffer()
    (if (not indent-tabs-mode)
        (untabify (point-min) (point-max)))
    nil)

;; Linum plugin
(require 'linum)
(line-number-mode   t)
(global-linum-mode  t)
(column-number-mode t)
(setq linum-format " %d")

;; Powerline config
(powerline-default-theme)
(setq powerline-height 20)
(setq powerline-raw " ")
(setq ns-use-srgb-colorspace nil)
(setq powerline-arrow-shape 'arrow)

(require 'font-lock+)
(require 'all-the-icons)

;; Disable backup/autosave files
(setq make-backup-files        nil)
(setq auto-save-default        nil)
(setq auto-save-list-file-name nil) ;; я так привык... хотите включить - замените nil на t

;; Syntax hightlighting
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer)
(global-set-key (kbd "<f2>") 'bs-show)

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan      t)
(setq imenu-use-popup-menu nil)
(global-set-key (kbd "<f6>") 'imenu) ;; вызов Imenu на F6

;; Font

(set-face-attribute 'default nil :font "Fira Mono for Powerline" )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
	 (quote
		("85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "d5b121d69e48e0f2a84c8e4580f0ba230423391a78fcb4001ccb35d02494d79e" "604648621aebec024d47c352b8e3411e63bdb384367c3dd2e8db39df81b475f5" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "d9129a8d924c4254607b5ded46350d68cc00b6e38c39fc137c3cfb7506702c12" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b97a01622103266c1a26a032567e02d920b2c697ff69d40b7d9956821ab666cc" default)))
 '(helm-mode t)
 '(package-selected-packages
	 (quote
		(base16-theme blackboard-theme popwin ranger flycheck js2-mode json-mode helm haskell-mode php-mode badwolf-theme solarized-theme kotlin-mode fringe-helper git-gutter-fringe+ monokai-theme spacemacs-theme git-gutter+ font-lock+ emmet-mode web-mode all-the-icons powerline jsx-mode coffee-mode goto-last-change evil neotree dracula-theme ## tagedit smex rainbow-delimiters projectile paredit magit ido-ubiquitous exec-path-from-shell clojure-mode-extra-font-locking cider)))
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
