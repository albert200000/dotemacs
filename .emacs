
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
(setq-default tab-width 2)
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
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
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

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Omit buffers starting with * when switching with C-x arrow
(set-frame-parameter (selected-frame) 'buffer-predicate
                     (lambda (buf) (not (string-match-p "^*" (buffer-name buf)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages
   '(typescript-mode dracula-theme indium diminish company-lsp deadgrep flimenu coffee-mode verb hl-todo all-the-icons-dired all-the-icons-ibuffer all-the-icons dumb-jump dotenv-mode company-web expand-region dired-sidebar yasnippet-snippets pug-mode format-all undo-fu yaml-mode avy logview company web-mode anzu magit php-mode dtrt-indent rainbow-mode json-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 110 :family "Hack")))))

(if (eq system-type 'darwin)
    (custom-set-faces
     '(default ((t (:height 150 :family "Hack")))))
  )

;; Auto install packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(load-theme 'dracula t)

;; Tell emacs where is your personal elisp lib dir
(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'icomplete)
(setq completion-ignore-case t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-max-delay-chars 0)
(setq icomplete-compute-delay 0)
(icomplete-mode t)
(fido-mode t)

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

(require 'dtrt-indent)
(dtrt-indent-global-mode t)

(require 'dumb-jump)
(dumb-jump-mode t)

(global-anzu-mode +1)

(require 'flymake)
(setq flymake-no-changes-timeout 0.2)
(flymake-mode t)
(flyspell-mode 0)

(require 'lsp-mode)
(setq lsp-enable-snippet nil)
(setq lsp-eldoc-render-all t)
(setq lsp-diagnostics-provider :flymake)
(setq lsp-modeline-diagnostics-enable t)
(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-ignored '("vendor" "node-modules"))
(setq lsp-log-io nil)
(setq lsp-enable-symbol-highlighting nil)

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.y?ml\\..*\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'lsp-deferred)

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\..*\\'" . json-mode))
(add-hook 'json-mode-hook 'lsp-deferred)

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
(add-to-list 'interpreter-mode-alist '("node" . web-mode))

(add-hook 'js-mode-hook (lambda ()
                          (lsp-deferred)
                          (setQuoteElectricPair)
                          ))

(add-hook 'web-mode-hook (lambda ()
                           (lsp-deferred)
                           (when (string-equal "twig" (file-name-extension buffer-file-name))
                             (set (make-local-variable 'company-backends) '(company-web-html company-dabbrev))
                             (company-mode t))))

(require 'pug-mode)
(setq pug-tab-width 2)

(add-hook 'pug-mode-hook (lambda ()
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
            (if (string-equal "js" (file-name-extension buffer-file-name))
              (require 'indium)
              (indium-interaction-mode +1))
            (when (or
                   (string-equal "js" (file-name-extension buffer-file-name))
                   (string-equal "tsx" (file-name-extension buffer-file-name))
                   )
              (setQuoteElectricPair)
              )))

(add-to-list 'magic-mode-alist '("^import React" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(require 'expand-region)

(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(require 'dired-sidebar)
(setq dired-sidebar-theme 'all-the-icons)
(setq dired-sidebar-use-custom-font t)

(all-the-icons-ibuffer-mode 1)

(global-hl-todo-mode t)

(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'flymake-mode)
(diminish 'dtrt-indent-mode)
(diminish 'anzu-mode)
(diminish 'company-mode)
(diminish 'subword-mode)
(diminish 'yas-minor-mode)
(diminish 'lsp-mode)
(diminish 'abbrev-mode)

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

(dir-locals-set-class-variables 'unwritable-directory
                                '((nil . ((web-mode-markup-indent-offset . 2)
                                          (web-mode-code-indent-offset . 2)
                                          (web-mode-attr-indent-offset . 2)))))

(dir-locals-set-directory-class
 "~/Projects/slot-web/" 'unwritable-directory)
(dir-locals-set-directory-class
 "~/Projects/slot-host/" 'unwritable-directory)
(dir-locals-set-directory-class
 "~/Projects/slot-node/" 'unwritable-directory)
(dir-locals-set-directory-class
 "~/Projects/slot-streaming-node/" 'unwritable-directory)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-r") 'imenu)
(global-set-key (kbd "C-S-s") 'deadgrep)
(global-set-key (kbd "C-t") 'project-find-file)
(global-set-key (kbd "C-S-r") 'dumb-jump-go-prompt)
(global-set-key (kbd "S-<return>") 'avy-goto-word-1)
(global-set-key (kbd "C-x .") 'dumb-jump-go)
(global-set-key (kbd "C-x ,") 'dumb-jump-back)
(global-set-key (kbd "C-z")   'undo-fu-only-undo)
(global-set-key (kbd "C-S-z") 'undo-fu-only-redo)
(global-set-key (kbd "C-\\") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "M-[") 'sp-unwrap-sexp)

;; Unboldify fonts
(set-face-attribute 'font-lock-type-face nil :weight 'normal)
(set-face-attribute 'font-lock-function-name-face nil :weight 'normal)
(set-face-attribute 'font-lock-keyword-face nil :weight 'normal)
(set-face-attribute 'font-lock-variable-name-face nil :weight 'normal)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)