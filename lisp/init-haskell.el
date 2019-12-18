;;; init-haskell.el --- Support the Haskell language -*- lexical-binding: t -*-
;;; Commentary: stolen from Steve Purcell: https://github.com/purcell/emacs.d/blob/master/lisp/init-haskell.el
;;; Code:

(when (require 'haskell-mode)
  (add-hook 'haskell-mode-hook 'subword-mode)
  (add-hook 'haskell-cabal-mode 'subword-mode)

  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

  ;; Indentation
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

  (add-hook 'haskell-mode-hook 'haskell-auto-insert-module-template))

(define-minor-mode stack-exec-path-mode
  "If this is a stack project, set `exec-path' to the path \"stack exec\" would use."
  nil
  :lighter ""
  :global nil
  (if stack-exec-path-mode
      (when (and (executable-find "stack")
                 (locate-dominating-file default-directory "stack.yaml"))
        (let ((stack-path (replace-regexp-in-string
                           "[\r\n]+\\'" ""
                           (shell-command-to-string (concat "stack exec -- sh -c "
                                                            (shell-quote-argument "echo $PATH"))))))
          (setq-local exec-path (seq-uniq (parse-colon-path stack-path) 'string-equal))
          (make-local-variable 'process-environment)
          (setenv "PATH" (string-join exec-path path-separator))))
    (kill-local-variable 'exec-path)
    (kill-local-variable 'process-environment)))

(add-hook 'haskell-mode-hook 'stack-exec-path-mode)

(provide 'init-haskell)

;;; init-haskell.el ends here

