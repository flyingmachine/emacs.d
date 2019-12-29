;; from https://github.com/purcell/emacs.d/blob/master/lisp/init-company.el

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(dolist (backend '(company-eclim company-semantic))
  (delq backend company-backends))

(define-key company-mode-map (kbd "M-/") 'company-complete)
(define-key company-active-map (kbd "M-/") 'company-other-backend)
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(setq-default company-dabbrev-other-buffers 'all
              company-tooltip-align-annotations t)
(global-set-key (kbd "M-C-/") 'company-complete)
