;;;;
;; Clojure
;;;;

;; (require 'cider)
;; (require 'clj-refactor)

;; turn off ansi in boot
(setenv "BOOT_COLOR" "off")

;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
;; (add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)


;; syntax hilighting and indentation
(setq clojure-indent-style :align-arguments)

(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (put-clojure-indent 'reg-event-db 1)
            (put-clojure-indent 'reg-event-fx 1)
            (put-clojure-indent 'reg-fx 1)
            (put-clojure-indent 'reg-cofx 1)
            (put-clojure-indent 'reg-sub 1)
            (put-clojure-indent 'bind-relations 1)
            (put-clojure-indent 'react-method 1)
            (put-clojure-indent 'render 1)
            (put-clojure-indent 'componentDidMount 1)
            (put-clojure-indent 'componentWillUnmount 1)
            (put-clojure-indent 'componentDidUpdate 1)
            (enable-paredit-mode)
            (subword-mode)))

(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojurescript-mode-hook #'aggressive-indent-mode)

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'eldoc-mode)
(add-hook 'cider-repl-mode-hook 'eldoc-mode)

;; autocomplete
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

;; ac-cider
;; (require 'ac-cider)
;; (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
;; (add-hook 'cider-mode-hook 'ac-cider-setup)
;; (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
;; (eval-after-load "auto-complete"
;;   '(progn
;;      (add-to-list 'ac-modes 'cider-mode)
;;      (add-to-list 'ac-modes 'cider-repl-mode)))
;; (defun set-auto-complete-as-completion-at-point-function ()
;;   (setq completion-at-point-functions '(auto-complete)))
;; (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
;; (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)


;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;;;;
;; clj-refactor
;;;;

(defun clj-refactor-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import
    (cljr-add-keybindings-with-prefix "C-c C-,"))

(add-hook 'clojure-mode-hook #'clj-refactor-hook)

;;;;
;; custom
;;;;

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
;; (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '(".cljs.hl$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))

;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "boot.user"))

(eval-after-load 'cider
  '(progn
     (define-key cider-mode-map (kbd "C-c r") 'cider-switch-to-repl-buffer)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-repl-mode-map (kbd "C-c u") 'cider-user-ns)))

(defun clj-align-vectors (beg end)
  (interactive "r")
  (align-regexp beg end "^ \\[[^ ]+\\(\\s-+\\)" 1 1 t))

