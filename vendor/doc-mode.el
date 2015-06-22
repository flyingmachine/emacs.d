;;; doc-mode.el --- a major-mode for highlighting a hierarchy structured text.

;; @Author: SUN, Tong <suntong001@users.sf.net>, (c)2001-6, all right reserved
;; @Version: $Date: 2006/01/19 03:13:41 $ $Revision: 1.14 $
;; @Keywords: text, processes, tools
;; @Home URL: http://xpt.sourceforge.net/
;; 
;; Distribute freely, but please include the author's info & copyright,
;; the file's version & url with the distribution.
;; 
;; Support free software movement! Please send you comments, suggestions, bug
;; reports, patches to the author from the xpt project home URL. They are
;; warmly welcome. Thank you for using the tools from the xpt project.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;{{{ Commentary: 

;;
;; To enter doc-mode automatically, add (autoload 'doc-mode "doc-mode")
;; to your .emacs file and change the first line of your doc file to:
;; # -*- Doc -*-
;; if it doesn't have a .doc extension.
;; 
;; To handle .doc files, e.g., 'filename.doc', add something like
;; (add-to-list 'auto-mode-alist '("\\.doc$" . doc-mode))
;; to your .emacs file

;;
;; The documentation for the "Structure Of The Hierarchy Text" can be found in
;; the docstring for the doc-mode function.
;;

;;}}}

;;{{{ Variables: 

(defgroup doc-faces nil
  "AsciiDoc highlighting"
  :group 'docs)

;; == Colors
; color n is more prominent than color n+1

(defface doc-title-1-face
  `((((class color)
      (background dark))
     (:foreground "brown3" :bold t :height 1.2 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "brown3" :bold t :height 1.2 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiDoc titles at level 1."
  :group 'doc-faces)

(defface doc-title-2-face
  `((((class color)
      (background dark))
     (:foreground "DeepPink2" :bold t :height 1.1 :inherit variable-pitch))
    (((class color)
      (background light))
     (:foreground "DeepPink2" :bold t :height 1.1 :inherit variable-pitch))
    (t (:weight bold :inherit variable-pitch)))
  "Face for AsciiDoc titles at level 2."
  :group 'doc-faces)

(defface doc-title-3-face
  `((((class color)
      (background dark))
     (:foreground "sienna3" :bold t))
    (((class color)
      (background light))
     (:foreground "sienna3" :bold t))
    (t (:weight bold)))
  "Face for AsciiDoc titles at level 3."
  :group 'doc-faces)

(defface doc-title-4-face
  `((((class color)
      (background dark))
     (:foreground "burlywood3"))
    (((class color)
      (background light))
     (:foreground "burlywood3"))
    (t ()))
  "Face for AsciiDoc titles at level 4."
  :group 'doc-faces)

(defface info-node
  '((((class color) (background light)) (:foreground "brown" :bold t :italic t))
    (((class color) (background dark)) (:foreground "white" :bold t :italic t))
    (t (:bold t :italic t)))
  "Face for Info node names."
  :group 'doc-faces)

(defvar doc-title-1 'doc-title-1-face)
(defvar doc-title-2 'doc-title-2-face)
(defvar doc-title-3 'doc-title-3-face)
(defvar doc-title-4 'doc-title-4-face)

(defvar general-font-lock-red1 font-lock-warning-face)
(defvar general-font-lock-red2 font-lock-comment-face)
(defvar general-font-lock-red3 font-lock-string-face)

(defvar general-font-lock-green1 font-lock-type-face)
(defvar general-font-lock-green2 font-lock-constant-face)

(defvar general-font-lock-blue1 font-lock-keyword-face)
(defvar general-font-lock-blue2 font-lock-function-name-face)
(defvar general-font-lock-blue3 font-lock-builtin-face)

(defvar general-font-lock-yellow1 font-lock-variable-name-face)
(defvar general-font-lock-yellow2 font-lock-comment-face)

;; == doc-mode settings

(defvar doc-mode-hook nil
  "Normal hook run when entering Doc Text mode.")

(defvar doc-mode-abbrev-table nil
  "Abbrev table in use in Doc-mode buffers.")
(define-abbrev-table 'doc-mode-abbrev-table ())

(defconst doc-font-lock-keywords
  (eval-when-compile
    (list

     ;; Section titles
     (cons "^==? +.*"		'doc-title-1)
     (cons "^=== +.*"		'doc-title-2)
     (cons "^==== +.*"		'doc-title-3)
     (cons "^===== +.*"		'doc-title-4)

     ;; Delimited blocks
     (cons "^[-=+*_]\\{6,\\} *\$" 'general-font-lock-yellow1)
     (cons "^\\.\\{6,\\} *\$"	'general-font-lock-yellow2)
     (cons "^\\.[A-Z].*\$"	'general-font-lock-yellow2)

     ;; Misc specials
     (cons "^.*\\?\\? *\$"	'doc-title-4)
     (cons "^.*[:;][:;-] *\$"	'general-font-lock-blue2)

     ;; Comment Lines
     (cons "^//.*"		'general-font-lock-blue3)

     ;; Auxiliary directives
     (cons "^>>[{}=]+ .*"	'general-font-lock-green1)
     (cons "^<< +.*"		'general-font-lock-blue3)

     ;; Annotation
     (cons "\\*[A-Z]+\\*:"	'general-font-lock-red1)

     ))
 "Default expressions to highlight in AsciiDoc mode.")

;;}}} 

;;{{{ Doc & Autoload: 

;;###autoload
(define-derived-mode doc-mode text-mode "DOC"
  "Major mode for editing AsciiDoc text files.
Turning on Doc mode runs the normal hook `doc-mode-hook'.

Document Structure

Titles consist of a line starting with one or more equals characters (the
number of equals corresponds the section level) followed by a space followed
by the title text. E.g.:

  = Document Title (level 0)
  == Section title (level 1)
  === Section title (level 2)
  ==== Section title (level 3)
  ===== Section title (level 4)

Auxiliary directives.

<< Last update time

>>{{{ folding start tag
>>}}} folding end tag
Group frames into folders.

Example:

== A Certain Unix Tool

=== Basic Info

==== Usage

// This is a hidden comment.

ListingBlocks are rendered verbatim in a monospaced font, they retain
line and whitespace formatting and often distinguished by a background
or border. There is no text formatting or substitutions within
Listing blocks apart from Special Characters and Callouts. Listing
blocks are often used for code and file listings.

--------------------------------------
#include <stdio.h>

int main() {
        printf(\"Hello World!\\n\");
        exit(0);
}
--------------------------------------

===== Description

===== Features

=== Version 

==== Dependencies

==== Installation

===== Steps

===== Help

<< 14:16:27
"
  (interactive)
  (modify-syntax-entry ?\'  ".")
  ;(flyspell-mode nil)

  (make-local-variable 'paragraph-start)
  (setq paragraph-start (concat "$\\|>" page-delimiter))
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate paragraph-start)
  (make-local-variable 'paragraph-ignore-fill-prefix)
  (setq paragraph-ignore-fill-prefix t)

  (make-local-variable 'require-final-newline)
  (setq require-final-newline t)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults 
	'(doc-font-lock-keywords 
	  nil				; KEYWORDS-ONLY: no
	  nil				; CASE-FOLD: no
	  ((?_ . "w"))			; SYNTAX-ALIST
	  ))
  (run-hooks 'doc-mode-hook))

(provide 'doc-mode)

;;}}}

;;; doc-mode.el ends here
