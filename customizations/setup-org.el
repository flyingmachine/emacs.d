;; The following lines are always needed.  Choose your own keys.
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(setq org-default-notes-file "~/Dropbox/org-life/notes.org")
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-indent-mode t)

;; bullets
(add-hook 'org-mode-hook 'org-bullets-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)
(setq org-bullets-bullet-list '("•"))

(setq org-capture-templates
      '(("q" "Quick note" entry (file+headline "~/Dropbox/org-life/master.org" "inbox")
         "* %? \n%i \n%a")
        ("t" "Todo" entry (file+headline "~/Dropbox/org-life/master.org" "inbox")
         "* TODO %?\n %i\n%a")
        ("d" "Debug journal" entry (file+datetree "~/Dropbox/org-life/debug-journal.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("n" "Note" entry (file "~/Dropbox/org-life/notes.org")
         "* %?")))
(setq org-startup-indented t)
(setq org-agenda-files (list "~/Dropbox/org-life/master.org"))
;; Configure refile to use ido and allow nested targets
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps nil)
(setq org-refile-use-outline-path t)
(setq org-refile-targets '((nil . (:maxlevel . 5))))
(setq org-capture-bookmark nil)

(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "|" "DONE")))
