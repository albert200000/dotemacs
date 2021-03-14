(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("GNU ELPA"     . 10)
        ("MELPA Stable" . 5)
        ("MELPA"        . 0)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setq read-process-output-max (* 1024 1024)) ;; 1mb
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq-default tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
;; The beeping can be annoying--turn it off
(setq visible-bell t
      ring-bell-function #'ignore)
;; Auto refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)
;; Also auto refresh Dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)
;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)
;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)
(setq mouse-wheel-scroll-amount '(2)) ;; lines at a time
(setq mouse-wheel-progressive-speed nil)
(setq scroll-step 1) ;; keyboard scroll lines at a time
(setq fast-but-imprecise-scrolling t)
(setq scroll-preserve-screen-position 'always)
(setq scroll-conservatively 101)
(setq auto-window-vscroll nil)
(setq scroll-margin 3)
(setq text-scale-mode-step '1.05)
(setq column-number-mode t)
(setq column-number-indicator-zero-based nil)
(setq line-number-mode t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq c-default-style "linux")
(desktop-save-mode 1)
;; normal shortcuts
(cua-mode t)
(setq-default cursor-type 'bar)
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (hs-minor-mode)
                            (diminish 'hs-minor-mode)
                            ))
(column-number-mode t)
(scroll-bar-mode t)
(tool-bar-mode -1)
(auto-save-visited-mode t)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(save-place-mode nil)
(global-subword-mode 1)
(delete-selection-mode 1)
(global-so-long-mode 1)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

(defun save-all ()
  (interactive)
  (save-some-buffers t))
(add-hook 'focus-out-hook 'save-all)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(windmove-default-keybindings 'meta)

(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)
(global-set-key (kbd "C-/") 'comment-line)
(global-set-key (kbd "C-x k") (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)
(global-set-key (kbd "C-x a") 'mark-whole-buffer)
(global-set-key (kbd "C-<tab>") 'indent-rigidly)
(global-set-key (kbd "S-<backspace>") 'delete-horizontal-space)
(global-set-key (kbd "<escape>") 'keyboard-quit)
;; zoom in/out like we do everywhere else.
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-_") 'text-scale-decrease)

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-command-modifier 'control))

;; Disabled *Completions*
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (let ((buffer "*Completions*"))
               (and (get-buffer buffer)
                    (kill-buffer buffer)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages
   '(move-text block-nav geben helm lsp-mode flymake-eslint typescript-mode dracula-theme indium diminish flimenu coffee-mode verb hl-todo all-the-icons-dired all-the-icons-ibuffer dumb-jump dotenv-mode company-web expand-region yasnippet-snippets pug-mode format-all undo-fu yaml-mode avy company web-mode anzu php-mode rainbow-mode)))

(set-frame-font "Hack:pixelsize=16")

(if (eq system-type 'darwin)
    (set-frame-font "Hack:pixelsize=16"))

;; Auto install packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(load-theme 'dracula t)

;; Custom lisp
(load "~/.emacs.d/lisp/flymake-pug")

(load "~/.emacs.d/lisp/guess-style")
(add-hook 'prog-mode-hook 'guess-style-guess-all)

(setq helm-boring-buffer-regexp-list (list (rx "*") (rx " markdown")))
(setq helm-split-window-inside-p t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-allow-mouse t)
(helm-mode 1)

(require 'paren)
(set-face-attribute 'show-paren-match nil :foreground "yellow")
(show-paren-mode t)

(require 'elec-pair)
(electric-pair-mode t)

(defun setQuoteElectricPair ()
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\'))))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

;; Causes problem with imenu in php mode
(semantic-mode -1)

(require 'imenu)
(setq imenu-max-item-length 1000)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 1000000)

(flimenu-global-mode 1)

(require 'dabbrev)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-upcase-means-case-search t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-require-match nil)
(setq company-idle-delay .1)
(require 'company-dabbrev)
(setq company-dabbrev-downcase nil)
(require 'company-web-html)

(require 'yasnippet)
(yas-global-mode 1)

(require 'dumb-jump)
(dumb-jump-mode t)

(global-anzu-mode +1)

(require 'flymake)
(setq flymake-no-changes-timeout 0.2)
(flyspell-mode 0)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (flymake-mode t)))

(require 'lsp-mode)
(setq lsp-enable-snippet nil)
(setq lsp-eldoc-render-all t)
(setq lsp-diagnostics-provider :flymake)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-diagnostics-disabled-modes '(web-mode))
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-ignored '("vendor" "node-modules"))
(setq lsp-log-io nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-folding nil)
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.y?ml\\..*\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'lsp-deferred)

(add-hook 'css-mode-hook 'lsp-deferred)
(add-hook 'scss-mode-hook 'lsp-deferred)

(require 'dotenv-mode) ; unless installed from a package
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-enable-auto-pairing nil)
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
(setq web-mode-markup-indent-offset 4)
(setq web-mode-code-indent-offset 4)
(setq web-mode-attr-indent-offset 4)
(add-to-list 'auto-mode-alist '("\\.min.js\\'" . text-mode))
(setq js-switch-indent-offset 2)
(add-to-list 'interpreter-mode-alist '("node" . js-mode))

(add-hook 'js-mode-hook (lambda ()
                          (when (string-equal "json" (file-name-extension buffer-file-name))
                            (lsp-deferred))
                          (when (string-equal "js" (file-name-extension buffer-file-name))
                            (flymake-eslint-enable))
                          (setQuoteElectricPair)
                          ))

(require 'pug-mode)
(setq pug-tab-width 2)

(add-hook 'pug-mode-hook (lambda ()
                           (flymake-pug-turn-on)
                           (setQuoteElectricPair)
                           (set (make-local-variable 'company-backends) '(company-web-jade company-dabbrev))
                           (company-mode t)))

(require 'php-mode)
(setq php-mode-coding-style 'psr2)

(add-hook 'php-mode-hook
          '(lambda ()
             (lsp-deferred)
             ))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name))
              (flymake-eslint-enable))
            (when (or
                   (string-equal "js" (file-name-extension buffer-file-name))
                   (string-equal "tsx" (file-name-extension buffer-file-name))
                   )
              (lsp-deferred)
              (setQuoteElectricPair)
              )
            (when (string-equal "twig" (file-name-extension buffer-file-name))
              (set (make-local-variable 'company-backends) '(company-web-html company-dabbrev))
              (company-mode t))))

(add-to-list 'magic-mode-alist '("^import React" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(require 'expand-region)

(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(all-the-icons-ibuffer-mode 1)

(global-hl-todo-mode t)

(setq org-support-shift-select t)

(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'flymake-mode)
(diminish 'anzu-mode)
(diminish 'company-mode)
(diminish 'subword-mode)
(diminish 'yas-minor-mode)
(diminish 'lsp-mode)
(diminish 'abbrev-mode)
(diminish 'all-the-icons-dired-mode)
(diminish 'all-the-icons-ibuffer-mode)
(diminish 'helm-mode)

(setq frame-title-format
      '(buffer-file-name "%f" ; File buffer
                         (dired-directory dired-directory ; Dired buffer
                                          (revert-buffer-function "%b" ; Buffer Menu
                                                                  (default-directory))))) ; Plain buffer

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

;; Auto indent on paste
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg))
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil)))))

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c s") 'helm-imenu)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-S-s") 'helm-grep-do-git-grep)
(global-set-key (kbd "C-c o") 'helm-occur)
(global-set-key (kbd "C-c f") 'project-find-file)
(global-set-key (kbd "S-<return>") 'avy-goto-word-1)
(global-set-key (kbd "C-x .") 'dumb-jump-go)
(global-set-key (kbd "C-x ,") 'dumb-jump-back)
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
(global-set-key (kbd "C-\\") 'speedbar)
(global-set-key (kbd "M-[") 'sp-unwrap-sexp)
(global-set-key (kbd "C-<down>") 'block-nav-next-block)
(global-set-key (kbd "C-<up>") 'block-nav-previous-block)
(global-set-key (kbd "C-S-<down>") 'block-nav-next-indentation-level)
(global-set-key (kbd "C-S-<up>") 'block-nav-previous-indentation-level)
(global-set-key (kbd "M-S-<down>") 'move-text-down)
(global-set-key (kbd "M-S-<up>") 'move-text-up)

;; Unboldify fonts
(set-face-attribute 'font-lock-type-face nil :weight 'normal)
(set-face-attribute 'font-lock-function-name-face nil :weight 'normal)
(set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
(set-face-attribute 'font-lock-variable-name-face nil :weight 'normal)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
