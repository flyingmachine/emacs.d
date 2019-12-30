;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
;; (add-to-list 'package-pinned-packages '(helm-projectile . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(flycheck . "melpa-stable") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)
(setq package-enable-at-startup nil)
;; (require 'use-package)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-context-switch-command nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; makes handling lisp expressions much, much easier
    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
    paredit

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
    clojure-mode

    ;; extra syntax highlighting for clojure
    clojure-mode-extra-font-locking

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
    cider

    ;; one of the best clojure packages
    clj-refactor

    ;; allow ido usage in as many contexts as possible. see
    ;; customizations/better-defaults.el line 47 for a description
    ;; of ido
    ido-ubiquitous

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
    smex

    ;; On OS X, an Emacs instance started from the graphical user
    ;; interface will have a different environment than a shell in a
    ;; terminal window, because OS X does not run a shell during the
    ;; login. Obviously this will lead to unexpected results when
    ;; calling external utilities like make from Emacs.
    ;; This library works around this problem by copying important
    ;; environment variables from the user's shell.
    ;; https://github.com/purcell/exec-path-from-shell
    exec-path-from-shell

    ;; project navigation
    projectile

    ;; colorful parenthesis matching
    rainbow-delimiters

    ;; edit html tags like sexps
    tagedit

    ;; git integration
    magit

    ;; https://github.com/bbatsov/crux
    crux

    yaml-mode
    coffee-mode
    scss-mode
    haskell-mode
    company
    ack-and-a-half
    ag
    adoc-mode
    aggressive-indent
    web-mode
    enh-ruby-mode
    base16-theme
    git-gutter
    helm
    helm-ag
    helm-git-grep
    helm-projectile
    helm-descbinds
    yasnippet
    use-package
    flycheck))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; helm
(load "setup-helm.el")

(load "setup-company.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-css.el")
(load "setup-yaml.el")
(load "setup-org.el")
(load "setup-prolog.el")
(load "setup-ruby.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#373b41"))
 '(cider-cljs-lein-repl
   "(do (require 'figwheel-sidecar.repl-api) (figwheel-sidecar.repl-api/start-figwheel!) (figwheel-sidecar.repl-api/cljs-repl))")
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("4486ade2acbf630e78658cd6235a5c6801090c2694469a2a2b4b0e12227a64b9" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#373b41")
 '(org-agenda-files (quote ("~/Dropbox/org-life/master.org")))
 '(package-selected-packages
   (quote
    (flycheck-clj-kondo flycheck typescript-mode terraform-mode jsx-mode chruby seeing-is-believing inf-ruby asciidoc cider sesman sass-mode swiper-helm counsel ivy powerline monokai-theme atom-one-dark-theme dracula-theme clojure-mode-extra-font-locking django-mode csv-mode csv doom-themes markdown-mode+ moe-theme org-bullets ac-cider base16-theme clojure-mode scss-mode yaml-mode web-mode use-package thesaurus tagedit smex rainbow-delimiters php-mode noflet markdown-mode magit ido-ubiquitous helm-projectile helm-ls-git helm-git-grep helm-descbinds helm-ag haskell-mode haml-mode git-gutter exec-path-from-shell enh-ruby-mode dockerfile-mode csharp-mode crux company color-theme-sanityinc-tomorrow coffee-mode clj-refactor aggressive-indent ag adoc-mode adjust-parens ack-and-a-half)))
 '(safe-local-variable-values
   (quote
    ((eval progn
           (put
            (quote defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote setting/defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote p\.types/defprotocol+)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (assert 1)
             (ex-info 1)
             (expect 0)
             (let-404 1)
             (match 1)
             (merge-with 1)
             (p\.types/defprotocol+
              (quote
               (1
                (:defn))))
             (p\.types/def-abstract-type
              (quote
               (1
                (:defn))))
             (p\.types/deftype+
              (quote
               (2 nil nil
                  (:defn))))
             (p\.types/defrecord+
              (quote
               (2 nil nil
                  (:defn))))))
     (eval progn
           (put
            (quote p\.types/defprotocol+)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (p\.types/defprotocol+
              (quote
               (1
                (:defn))))
             (p\.types/definterface+
              (quote
               (1
                (:defn))))
             (p\.types/def-abstract-type
              (quote
               (1
                (:defn))))
             (p\.types/deftype+
              (quote
               (2 nil nil
                  (:defn))))
             (p\.types/defrecord+
              (quote
               (2 nil nil
                  (:defn))))))
     (eval progn
           (put
            (quote defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote setting/defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote p\.types/defprotocol+)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (assert 1)
             (ex-info 1)
             (expect 0)
             (let-404 1)
             (match 1)
             (merge-with 1)
             (with-redefs-fn 1)
             (p\.types/defprotocol+
              (quote
               (1
                (:defn))))
             (p\.types/def-abstract-type
              (quote
               (1
                (:defn))))
             (p\.types/deftype+
              (quote
               (2 nil nil
                  (:defn))))
             (p\.types/defrecord+
              (quote
               (2 nil nil
                  (:defn))))))
     (eval progn
           (put
            (quote defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote setting/defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (assert 1)
             (assoc 1)
             (ex-info 1)
             (expect 0)
             (match 1)
             (merge-with 1)
             (with-redefs-fn 1)))
     (cljr-favor-prefix-notation . t)
     (eval progn
           (put
            (quote defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote api/defendpoint-async)
            (quote clojure-doc-string-elt)
            3)
           (put
            (quote defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote setting/defsetting)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote s/defn)
            (quote clojure-doc-string-elt)
            2)
           (put
            (quote p\.types/defprotocol+)
            (quote clojure-doc-string-elt)
            2)
           (define-clojure-indent
             (assert 1)
             (ex-info 1)
             (expect 0)
             (match 1)
             (merge-with 1)
             (with-redefs-fn 1)
             (p\.types/defprotocol+
              (quote
               (1
                (:defn))))
             (p\.types/def-abstract-type
              (quote
               (1
                (:defn))))
             (p\.types/deftype+
              (quote
               (2 nil nil
                  (:defn))))
             (p\.types/defrecord+
              (quote
               (2 nil nil
                  (:defn))))))
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-refresh-after-fn . "integrant.repl/resume")
     (cider-refresh-before-fn . "integrant.repl/suspend"))))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(org-level-2 ((t (:foreground "#5fb3b3"))))
;;  '(org-level-4 ((t (:foreground "#ddc794")))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
