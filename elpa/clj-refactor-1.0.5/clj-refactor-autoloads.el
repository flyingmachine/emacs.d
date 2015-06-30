;;; clj-refactor-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "clj-refactor" "clj-refactor.el" (21900 990
;;;;;;  0 0))
;;; Generated autoloads from clj-refactor.el

(autoload 'cljr-add-keybindings-with-prefix "clj-refactor" "\


\(fn PREFIX)" nil nil)

(autoload 'cljr-add-keybindings-with-modifier "clj-refactor" "\


\(fn MODIFIER)" nil nil)

(autoload 'cljr-rename-file "clj-refactor" "\
Renames current buffer and file it is visiting.

\(fn)" t nil)

(autoload 'cljr-sort-ns "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-remove-unused-requires "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-add-require-to-ns "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-add-use-to-ns "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-add-import-to-ns "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-replace-use "clj-refactor" "\
Replace any :use clause with the equivalent :require clause.

Presently, there's no support for :use clauses containing :exclude.

\(fn)" t nil)

(autoload 'cljr-stop-referring "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-move-form "clj-refactor" "\
Move the form containing POINT to a new namespace.

If REGION is active, move all forms contained by region. 

\(fn)" t nil)

(autoload 'cljr-add-declaration "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-cycle-thread "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-unwind "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-unwind-all "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-thread "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-thread-first-all "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-thread-last-all "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-introduce-let "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-expand-let "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-move-to-let "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-destructure-keys "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-cycle-privacy "clj-refactor" "\


\(fn)" t nil)

(autoload 'cljr-cycle-stringlike "clj-refactor" "\
Removed, use `clojure-toggle-keyword-string'

\(fn)" t nil)

(autoload 'cljr-cycle-coll "clj-refactor" "\
Convert the coll at (point) from (x) -> {x} -> [x] -> -> #{x} -> (x) recur

\(fn)" t nil)

(autoload 'cljr-cycle-if "clj-refactor" "\
Cycle surrounding if or if-not, to if-not or if

\(fn)" t nil)

(autoload 'cljr-raise-sexp "clj-refactor" "\
Like paredit-raise-sexp, but removes # in front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-splice-sexp-killing-backward "clj-refactor" "\
Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-splice-sexp-killing-forward "clj-refactor" "\
Like paredit-splice-sexp-killing-backward, but removes # in
front of function literals and sets.

\(fn &optional ARGUMENT)" t nil)

(autoload 'cljr-slash "clj-refactor" "\
Inserts / as normal, but also checks for common namespace shorthands to require.

\(fn)" t nil)

(autoload 'cljr-project-clean "clj-refactor" "\
Runs `cljr-project-clean-functions' on every clojure file, then
sorts the project's dependency vectors.

\(fn)" t nil)

(autoload 'cljr-sort-project-dependencies "clj-refactor" "\


\(fn)" t nil)

(autoload 'clj-refactor-mode "clj-refactor" "\
A mode to keep the clj-refactor keybindings.

\(fn &optional ARG)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; clj-refactor-autoloads.el ends here
