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
(setq mode-require-final-newline nil)
(fset 'yes-or-no-p 'y-or-n-p)
;; The beeping can be annoying--turn it off
(setq visible-bell t ring-bell-function #'ignore)
;; Auto refresh buffers when edits occur outside emacs
(global-auto-revert-mode 1)
(require 'autorevert)
(setq global-auto-revert-non-file-buffers t) ;; Also auto refresh Dired, but be quiet about it
(setq auto-revert-verbose nil)
(setq delete-by-moving-to-trash t) ;; Move files to trash when deleting
(setq inhibit-compacting-font-caches t) ;; Don’t compact font caches during GC.
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
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default standard-indent 4)
(desktop-save-mode 1)
(cua-mode t) ;; normal shortcuts
(setq-default cursor-type 'bar)
(column-number-mode t)
(scroll-bar-mode t)
(tool-bar-mode -1)
(auto-save-visited-mode t)
(setq create-lockfiles nil)
(recentf-mode 1)
(require 'recentf)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(save-place-mode nil)
(global-subword-mode 1)
(delete-selection-mode 1)
(global-so-long-mode 1)
(global-hl-line-mode -1)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(add-function :after after-focus-change-function (lambda () (unless (frame-focus-state) (save-some-buffers t))))

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
   '(flimenu deadgrep ido-completing-read+ editorconfig move-text block-nav lsp-mode flymake-eslint typescript-mode diminish coffee-mode verb hl-todo all-the-icons-dired all-the-icons-ibuffer dumb-jump dotenv-mode company-web expand-region pug-mode format-all undo-fu yaml-mode avy company web-mode anzu php-mode rainbow-mode)))

(set-frame-font "Hack:pixelsize=16")

(if (eq system-type 'darwin)
    (set-frame-font "Hack:pixelsize=16"))

;; Auto install packages
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(load-theme 'deeper-blue t)

;; Custom lisp
(load "~/.emacs.d/lisp/flymake-pug")
(load "~/.emacs.d/lisp/guess-style")

(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode)
                            (hs-minor-mode)
                            (diminish 'hs-minor-mode)
                            (guess-style-guess-all)
                            ))

(require 'ido)
(setq ido-everywhere t)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-create-new-buffer 'always)
(setq ido-ignore-buffers '("\\` " "^\*"))
(ido-mode t)

(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

(require 'paren)
(show-paren-mode t)

(require 'elec-pair)
(electric-pair-mode t)

(defun setQuoteElectricPair ()
  (setq-local electric-pair-pairs (append electric-pair-pairs '((?\' . ?\'))))
  (setq-local electric-pair-text-pairs electric-pair-pairs))

(semantic-mode -1) ;; Causes problem with imenu in php mode

(require 'imenu)
(setq imenu-max-item-length 1000)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 1000000)

(require 'flimenu)
(flimenu-global-mode)

(require 'dabbrev)
(setq dabbrev-case-fold-search nil)
(setq dabbrev-upcase-means-case-search t)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-require-match nil)
(setq company-idle-delay 0.0)
(setq company-minimum-prefix-length 2)
(setq company-selection-wrap-around t)
(require 'company-dabbrev)
(setq company-dabbrev-downcase nil)
(require 'company-web-html)

(require 'dumb-jump)
(dumb-jump-mode t)

(global-anzu-mode +1)

(require 'flymake)
(setq flymake-no-changes-timeout 0.2)
(flyspell-mode 0)

(add-hook 'emacs-lisp-mode-hook #'flymake-mode)

(require 'lsp-mode)
(setq lsp-enable-snippet nil)
(setq lsp-eldoc-render-all t)
(setq lsp-diagnostics-provider :flymake)
(setq lsp-modeline-diagnostics-enable nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-enable-file-watchers nil)
(setq lsp-file-watch-ignored '("vendor" "node-modules"))
(setq lsp-log-io nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-folding nil)
(add-to-list 'lsp-language-id-configuration '(js-jsx-mode . "javascript"))
(setenv "TSSERVER_LOG_FILE" "/tmp/tsserver.log")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.y?ml\\..*\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'lsp-deferred)

(add-hook 'css-mode-hook 'lsp-deferred)
(add-hook 'scss-mode-hook 'lsp-deferred)

(require 'dotenv-mode) ; unless installed from a package
;; for optionally supporting additional file extensions such as `.env.test' with this major mode
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig\\'" . web-mode))
(setq web-mode-enable-auto-pairing nil)
(setq web-mode-enable-auto-indentation nil)
(add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
(add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))

(add-hook 'js-mode-hook (lambda ()
                          (when (string-equal "js" (file-name-extension buffer-file-name))
                            (setq lsp-diagnostics-disabled-modes '(js-mode js-jsx-mode))
                            (flymake-eslint-enable))
                          (lsp-deferred)
                          (setQuoteElectricPair)
                          ))

(add-hook 'typescript-mode-hook (lambda ()
                                  (lsp-deferred)
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
            (setQuoteElectricPair)
            (set (make-local-variable 'company-backends) '(company-web-html company-dabbrev))
            (company-mode t)))

(add-to-list 'auto-mode-alist '("\\.min.js\\'" . text-mode))
;; for optionally supporting additional file extensions such as `.json.test' with this major mode
(add-to-list 'auto-mode-alist '("\\.json\\..*\\'" . js-mode))
(add-to-list 'interpreter-mode-alist '("node" . js-mode))

(require 'expand-region)

(require 'all-the-icons)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(all-the-icons-ibuffer-mode 1)
(global-hl-todo-mode t)
(editorconfig-mode 1)

(setq org-support-shift-select t)

(require 'diminish)
(diminish 'eldoc-mode)
(diminish 'flymake-mode)
(diminish 'anzu-mode)
(diminish 'company-mode)
(diminish 'subword-mode)
(diminish 'lsp-mode)
(diminish 'abbrev-mode)
(diminish 'all-the-icons-dired-mode)
(diminish 'all-the-icons-ibuffer-mode)
(diminish 'editorconfig-mode)

(setq frame-title-format
      '(buffer-file-name "%f" ; File buffer
                         (dired-directory dired-directory ; Dired buffer
                                          (revert-buffer-function "%b" ; Buffer Menu
                                                                  (default-directory))))) ; Plain buffer

(dir-locals-set-class-variables 'react-directory
                                '((nil . ((mode . js-jsx)))))

(dir-locals-set-directory-class
 "~/Projects/slot-web-new/src/components" 'react-directory)

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
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "C-c s") 'imenu)
(global-set-key (kbd "C-S-s") 'deadgrep)
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
