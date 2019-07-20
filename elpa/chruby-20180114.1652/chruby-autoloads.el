;;; chruby-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "chruby" "chruby.el" (0 0 0 0))
;;; Generated autoloads from chruby.el

(autoload 'chruby "chruby" "\
If name is given, activate the given Ruby. Else, return the currently
 activated Ruby

\(fn &optional NAME)" nil nil)

(autoload 'chruby-use "chruby" "\
choose what ruby you want to activate

\(fn RUBY-VERSION)" t nil)

(autoload 'chruby-use-corresponding "chruby" "\
search for .ruby-version and activate the corresponding ruby

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "chruby" '("chruby-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; chruby-autoloads.el ends here
