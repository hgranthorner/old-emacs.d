;;; package --- Summary
;;; Commentary:
;;; Code:
;; Set up Melpa
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines for stable
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives
  ;;(cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Initialize use package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package diminish)

;; Update packages regularly
(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

;; Set path on mac
(use-package exec-path-from-shell
  :config
  (when is-mac (exec-path-from-shell-initialize)))

(use-package gruber-darker-theme)


;; Sensible startup
(defalias 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(set-frame-font "Ubuntu Mono 17" nil t)
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
(setq mac-command-modifier 'control)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(setq-default indent-tabs-mode nil)

;; Dired configuration
(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-x")))
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;; Org mode
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "IN PROGRESS" "|" "DONE" "DELEGATED")))

;; Autocomplete brackets
(electric-pair-mode 1)
(show-paren-mode 1)
(setq electric-pair-preserve-balance nil)

;; Fix c indentation
(defun fix-c-indent-offset-according-to-syntax-context (key val)
  "Fix c braces KEY VAL."
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  (add-to-list 'c-offsets-alist '(key . val)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              ;; indent
              (fix-c-indent-offset-according-to-syntax-context 'substatement-open 0))))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  ("C->" . 'mc/mark-next-like-this)
  ("C-<" . 'mc/mark-previous-like-this)
  ("C-c C-<" . 'mc/mark-all-like-this))

;; Turn on company mode
(use-package company
  :diminish company-mode
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; Flycheck config
(use-package flycheck
  :defer t
  :diminish flycheck-mode
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'typescript-tslint 'rjsx-mode))

(use-package flycheck-clang-analyzer
  :defer t
  :ensure t
  :config
  (flycheck-clang-analyzer-setup))

(use-package haskell-mode
  :mode (("\\.hs\\'"    . haskell-mode)
         ("\\.cabal\\'" . haskell-cabal-mode)
         ("\\.hcr\\'"   . haskell-core-mode))
  :interpreter ("haskell" . haskell-mode)
  :init
  (add-hook 'haskell-mode-hook 'structured-haskell-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package magit
  :bind ("C-x g" . 'magit-status))

;; Ivy
(use-package ivy
  :diminish ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-display-style 'fancy)
  :config
  (ivy-mode 1)
  :bind ("C-x b" . 'ivy-switch-buffer))

(use-package counsel
  :diminish counsel-mode
  :config (counsel-mode 1))

(use-package swiper
  :bind
  ("C-s" . 'swiper)
  ("C-r" . 'swiper))

(use-package doom-modeline
  :init (setq doom-modeline-major-mode-icon nil)
  :hook (after-init . doom-modeline-mode))
1
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (gruber-darker)))
 '(custom-safe-themes
   (quote
    ("47ec21abaa6642fefec1b7ace282221574c2dd7ef7715c099af5629926eb4fd7" default)))
 '(flycheck-emacs-lisp-load-path (quote inherit))
 '(package-selected-packages
   (quote
    (use-package browse-kill-ring yasnippet-snippets yasnippet move-text multiple-cursors exec-path-from-shell flymake flymake-hlint haskell-mode magit company counsel ivy gruber-darker-theme smex)))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#e4e4ef" :background "#181818")))))

(provide 'init)
;;; init.el ends here
