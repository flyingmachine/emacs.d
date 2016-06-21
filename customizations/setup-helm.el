;;; init-helm-thierry.el --- My startup file for helm. -*- lexical-binding: t -*- 
;;; Code:

;;; Load all autoloads for helm and its extensions
;;
(require 'helm-config)
;;(load "/home/thierry/elisp/helm-extensions/helm-extensions-autoloads.el")


;;; Enable Modes (helm-mode is loading nearly everything).
;;
(use-package helm-mode
    :config (helm-mode 1))

(use-package helm-adaptive
    :config (helm-adaptive-mode 1))

(use-package helm-ring
    :config (helm-push-mark-mode 1))

(use-package helm-utils
    ;; Popup buffer-name or filename in grep/moccur/imenu-all etc...
    :config (helm-popup-tip-mode 1))

;; (use-package helm-sys
;;     :config (helm-top-poll-mode 1))

;;;; Test Sources or new helm code. 
;;   !!!WARNING EXPERIMENTAL!!!

(defun helm/version-1 ()
  (with-temp-buffer
    (insert-file-contents (find-library-name "helm-core-pkg"))
    (goto-char (point-min))
    (when (re-search-forward
           "\\([0-9]+?\\)\\.?\\([0-9]*\\)\\.?\\([0-9]*\\)\\.?[0-9]*" nil t)
      (match-string-no-properties 0))))

;; Helm version: 1.9.3
(defun helm/version (arg)
  (interactive "P")
  (let ((version-str (format "Helm version: %s" (helm/version-1))))
    (if arg (insert version-str) (message version-str))))

(defun helm/git-version ()
  (shell-command-to-string
   "git log --pretty='format:%H' -1"))

(defun helm/turn-on-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line t)
  (setq helm-split-window-in-side-p t)
  (helm-autoresize-mode -1)
  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/turn-off-header-line ()
  (interactive)
  (setq helm-echo-input-in-header-line nil)
  ;;(helm-autoresize-mode 1)
  (setq helm-split-window-in-side-p nil)
  (remove-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)
  )

(defun helm/occur-which-func ()
  (interactive)
  (with-current-buffer
      (or (helm-aif (with-helm-buffer
                      (window-buffer helm-persistent-action-display-window))
              (and (null (minibufferp it)) it))
          helm-current-buffer)
    (when (eq major-mode 'emacs-lisp-mode)
      (message "[%s]" (which-function)))))

(define-key helm-moccur-map (kbd "C-c ?") 'helm/occur-which-func)
(define-key helm-grep-map   (kbd "C-c ?") 'helm/occur-which-func)

;; Show the visibles buffers on top of list (issue #1301)

(defun helm/modify-ido-temp-list ()
  (let ((bl (mapcar #'buffer-name (buffer-list (selected-frame)))))
    (setq ido-temp-list (nconc (cdr bl) (list (car bl))))))
;;(add-hook 'ido-make-buffer-list-hook 'helm/modify-ido-temp-list)


;;; Helm-command-map
;;
;;
(define-key helm-command-map (kbd "g") 'helm-apt)
(define-key helm-command-map (kbd "z") 'helm-complex-command-history)
(define-key helm-command-map (kbd "w") 'helm-w3m-bookmarks)
(define-key helm-command-map (kbd "x") 'helm-firefox-bookmarks)
(define-key helm-command-map (kbd "#") 'helm-emms)
(define-key helm-command-map (kbd "I") 'helm-imenu-in-all-buffers)

;;; Global-map
;;
;;
(global-set-key (kbd "M-x")                          'undefined)
(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "M-y")                          'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)
(global-set-key (kbd "C-c <SPC>")                    'helm-all-mark-rings)
(global-set-key (kbd "C-x r b")                      'helm-filtered-bookmarks)
(global-set-key (kbd "C-h r")                        'helm-info-emacs)
(global-set-key (kbd "C-:")                          'helm-eval-expression-with-eldoc)
(global-set-key (kbd "C-,")                          'helm-calcul-expression)
(global-set-key (kbd "C-h i")                        'helm-info-at-point)
(global-set-key (kbd "C-x C-d")                      'helm-browse-project)
(global-set-key (kbd "<f1>")                         'helm-resume)
(global-set-key (kbd "C-h C-f")                      'helm-apropos)
(global-set-key (kbd "C-h a")                        'helm-apropos)
(global-set-key (kbd "<f5> s")                       'helm-find)
(global-set-key (kbd "<f2>")                         'helm-execute-kmacro)
(global-set-key (kbd "C-c i")                        'helm-imenu-in-all-buffers)
;; (global-set-key (kbd "<f11> o")                      'helm-org-agenda-files-headings)
(global-set-key (kbd "C-s")                          'helm-occur)
(define-key global-map [remap jump-to-register]      'helm-register)
(define-key global-map [remap list-buffers]          'helm-mini)
(define-key global-map [remap dabbrev-expand]        'helm-dabbrev)
(define-key global-map [remap find-tag]              'helm-etags-select)
(define-key global-map [remap xref-find-definitions] 'helm-etags-select)
(define-key global-map (kbd "M-g a")                 'helm-do-grep-ag)
(define-key global-map (kbd "M-g g")                 'helm-grep-do-git-grep)
(define-key global-map (kbd "M-g i")                 'helm-gid)

(helm-multi-key-defun helm-multi-lisp-complete-at-point
    "Multi key function for completion in emacs lisp buffers.
First call indent, second complete symbol, third complete fname."
  '(helm-lisp-indent
    helm-lisp-completion-at-point
    helm-complete-file-name-at-point)
  0.3)

(if (and (boundp 'tab-always-indent)
         (eq tab-always-indent 'complete)
         (boundp 'completion-in-region-function))
    (progn
      (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
      (define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
      
      ;; lisp complete. (Rebind M-<tab>)
      (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
      (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
    
    (define-key lisp-interaction-mode-map [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
    (define-key emacs-lisp-mode-map       [remap indent-for-tab-command] 'helm-multi-lisp-complete-at-point)
    
    ;; lisp complete. (Rebind M-<tab>)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(unless (boundp 'completion-in-region-function)
  (add-hook 'ielm-mode-hook
            #'(lambda ()
                (define-key ielm-map [remap completion-at-point] 'helm-lisp-completion-at-point))))

;;; helm find files
;;
(define-key helm-find-files-map (kbd "C-d") 'helm-ff-persistent-delete)
(define-key helm-buffer-map (kbd "C-d")     'helm-buffer-run-kill-persistent)
(define-key helm-find-files-map (kbd "C-/") 'helm-ff-run-find-sh-command)

;; Use default-as-input in grep
(add-to-list 'helm-sources-using-default-as-input 'helm-source-grep)
(add-to-list 'helm-sources-using-default-as-input 'helm-source-grep-ag)

;;; Describe key-bindings
;;
;;
(helm-descbinds-install)            ; C-h b, C-x C-h


;;; Helm-variables
;;
;;
(setq helm-net-prefer-curl                            t
      helm-kill-ring-threshold                        1
      helm-raise-command                              "wmctrl -xa %s"
      helm-scroll-amount                              4
      helm-idle-delay                                 0.01
      helm-input-idle-delay                           0.01
      helm-default-external-file-browser              "thunar"
      helm-pdfgrep-default-read-command               "evince --page-label=%p '%f'"
      helm-ff-auto-update-initial-value               t
      helm-grep-default-command                       "grep -Hn --color --smart-case --no-group %e %p %f"
      helm-grep-default-recurse-command               "grep -H --color --smart-case --no-group %e %p %f"
      helm-reuse-last-window-split-state              t
      helm-always-two-windows                         t
      helm-split-window-in-side-p                     nil
      helm-buffers-favorite-modes                     (append helm-buffers-favorite-modes
                                                              '(picture-mode artist-mode))
      helm-ls-git-status-command                      'magit-status-internal
      helm-M-x-requires-pattern                       0
      helm-dabbrev-cycle-threshold                    5
      helm-surfraw-duckduckgo-url
      "https://duckduckgo.com/?q=%s&ke=-1&kf=fw&kl=fr-fr&kr=b&k1=-1&k4=-1"
      helm-boring-file-regexp-list
      '("\\.git$" "\\.hg$" "\\.svn$" "\\.CVS$" "\\._darcs$" "\\.la$" "\\.o$" "\\.i$" "\\.$")
      helm-buffer-skip-remote-checking                t
      helm-apropos-fuzzy-match                        t
      helm-M-x-fuzzy-match                            t
      helm-lisp-fuzzy-completion                      t
      helm-completion-in-region-fuzzy-match           t
      helm-buffers-fuzzy-matching                     t
      helm-move-to-line-cycle-in-source               t
      ido-use-virtual-buffers                         t             ; Needed in helm-buffers-list
      helm-tramp-verbose                              6
      helm-locate-command                             "locate %s -e -A --regex %s"
      helm-org-headings-fontify                       t
      helm-autoresize-max-height                      80 ; it is %.
      helm-autoresize-min-height                      20 ; it is %.
      helm-buffers-to-resize-on-pa                    '("*helm apropos*" "*helm ack-grep*"
                                                        "*helm grep*" "*helm occur*" "*helm ag*"
                                                        "*helm multi occur*" "*helm git-grep*"
                                                        "*helm imenu*" "*helm imenu all*"
                                                        "*helm gid*" "*helm semantic/imenu*")
      fit-window-to-buffer-horizontally               1
      helm-open-github-closed-issue-since             7
      helm-highlight-matches-around-point-max-lines   30
      helm-search-suggest-action-wikipedia-url
      "https://fr.wikipedia.org/wiki/Special:Search?search=%s"
      helm-wikipedia-suggest-url
      "https://fr.wikipedia.org/w/api.php?action=opensearch&search="
      helm-wikipedia-summary-url
      "https://fr.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page="
      helm-firefox-show-structure nil
      helm-turn-on-recentf t
      helm-mini-default-sources '(helm-source-buffers-list helm-source-buffer-not-found)
      helm-ff-skip-boring-files t)

;; Avoid hitting forbidden directory .gvfs when using find.
(add-to-list 'completion-ignored-extensions ".gvfs/")


;;; Toggle grep program
;;
;;
(defun helm/eselect-grep ()
  (interactive)
  (when (y-or-n-p (format "Current grep program is %s, switching? "
                          (helm-grep-command)))
    (if (helm-grep-use-ack-p)
        (setq helm-grep-default-command
              "grep --color=always -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "grep --color=always -d recurse %e -n%cH -e %p %f")
        (setq helm-grep-default-command
              "ack-grep -Hn --color --smart-case --no-group %e %p %f"
              helm-grep-default-recurse-command
              "ack-grep -H --color --smart-case --no-group %e %p %f"))
    (message "Switched to %s" (helm-grep-command))))

;;; Debugging
;;
;;
(defun helm/debug-toggle ()
  (interactive)
  (setq helm-debug (not helm-debug))
  (message "Helm Debug is now %s"
           (if helm-debug "Enabled" "Disabled")))

(defun helm/ff-candidates-lisp-p (candidate)
  (cl-loop for cand in (helm-marked-candidates)
           always (string-match "\.el$" cand)))


;;; Modify source attributes
;;
;; Add actions to `helm-source-find-files' IF:

(defmethod helm-setup-user-source ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Byte compile file(s) async"
   'tv/async-byte-compile-file
   source
   'helm/ff-candidates-lisp-p))

(defmethod helm-setup-user-source ((source helm-source-buffers))
  (setf (slot-value source 'candidate-number-limit) 300))


;;; helm dictionary
;;
(setq helm-dictionary-database "~/helm-dictionary/dic-en-fr.iso")
(setq helm-dictionary-online-dicts '(("translate.reference.com en->fr" .
                                      "http://translate.reference.com/translate?query=%s&src=en&dst=fr")
                                     ("translate.reference.com fr->en" .
                                      "http://translate.reference.com/translate?query=%s&src=fr&dst=en")))

;; projectile
(with-eval-after-load 'projectile
  (helm-projectile-on)
  (setq projectile-switch-project-action #'helm-projectile)
  (setq projectile-completion-system 'helm))

;; more custom
(setq helm-ff-newfile-prompt-p nil)
;; enter to nav
(defun fu/helm-find-files-navigate-forward (orig-fun &rest args)
  (if (file-directory-p (helm-get-selection))
      (apply orig-fun args)
    (helm-maybe-exit-minibuffer)))
(advice-add 'helm-execute-persistent-action :around #'fu/helm-find-files-navigate-forward)
(define-key helm-find-files-map (kbd "<return>") 'helm-execute-persistent-action)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-c g") 'helm-git-grep)

;; custom keys
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z")  'helm-select-action)

(provide 'init-helm-thierry)

;;; init-helm-thierry.el ends here
