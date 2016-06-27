;;; gruvbox-dark-theme.el --- A retro groove color theme for GNU Emacs

;; Copyright (c) 2016 Dario Gjorgjevski

;; Author: Dario Gjorgjevski <dario.gjorgjevski@gmail.com>
;; URL: http://github.com/d125q/gruvbox-dark-emacs
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The Gruvbox colors ported to GNU Emacs.  Built on top of the new built-in
;; theme support in GNU Emacs 24.

;;; Credits:

;; Pavel Pertsev created the original Gruvbox theme for the Vim editor.  This
;; port is based entirely on his work.

;;; Code:

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))

(deftheme gruvbox-dark "A retro groove color theme for GNU Emacs")

(let* ((get-color (lambda (graphic-color terminal-color)
                    "Switch between GRAPHIC-COLOR (for GUI) and TERMINAL-COLOR (for TTY)."
                    (if (display-graphic-p) graphic-color terminal-color)))
       (dark0_h  (funcall get-color "#1d2021" "color-234"))
       (dark0    (funcall get-color "#282828" "color-235"))
       (dark0_s  (funcall get-color "#32302f" "color-236"))
       (dark1    (funcall get-color "#3c3836" "color-237"))
       (dark2    (funcall get-color "#504945" "color-239"))
       (dark3    (funcall get-color "#665c54" "color-241"))
       (dark4    (funcall get-color "#7c6f64" "color-243"))

       (light0_h (funcall get-color "#ffffc8" "color-230"))
       (light0   (funcall get-color "#fdf4c1" "color-229"))
       (light0_s (funcall get-color "#f4e8ba" "color-228"))
       (light1   (funcall get-color "#ebdbb2" "color-223"))
       (light2   (funcall get-color "#d5c4a1" "color-250"))
       (light3   (funcall get-color "#bdae93" "color-248"))
       (light4   (funcall get-color "#a89984" "color-246"))

       (bright_red    (funcall get-color "#fb4933" "color-167"))
       (bright_green  (funcall get-color "#b8bb26" "color-142"))
       (bright_yellow (funcall get-color "#fabd2f" "color-214"))
       (bright_blue   (funcall get-color "#83a598" "color-109"))
       (bright_purple (funcall get-color "#d3869b" "color-175"))
       (bright_aqua   (funcall get-color "#8ec07c" "color-108"))
       (bright_orange (funcall get-color "#fe8019" "color-208"))
       (bright_gray   (funcall get-color "#a89984" "color-246"))

       (neutral_red    (funcall get-color "#cc241d" "color-124"))
       (neutral_green  (funcall get-color "#98971a" "color-106"))
       (neutral_yellow (funcall get-color "#d79921" "color-172"))
       (neutral_blue   (funcall get-color "#458588" "color-74"))
       (neutral_purple (funcall get-color "#b16286" "color-132"))
       (neutral_aqua   (funcall get-color "#689d6a" "color-72"))
       (neutral_orange (funcall get-color "#d65d0e" "color-166"))
       (neutral_gray   (funcall get-color "#928374" "color-245"))

       (faded_red    (funcall get-color "#9d0006" "color-88"))
       (faded_green  (funcall get-color "#79740e" "color-100"))
       (faded_yellow (funcall get-color "#b57614" "color-136"))
       (faded_blue   (funcall get-color "#076678" "color-24"))
       (faded_purple (funcall get-color "#8f3f71" "color-96"))
       (faded_aqua   (funcall get-color "#427b58" "color-66"))
       (faded_orange (funcall get-color "#af3a03" "color-130"))
       (faded_gray   (funcall get-color "#7c6f64" "color-243")))

  (custom-theme-set-faces
   'gruvbox-dark

   ;; basic coloring
   '(button              ((t (:underline t))))
   `(cursor              ((t (:background ,light1))))
   `(default             ((t (:background ,dark0 :foreground ,light1))))
   `(error               ((t (:foreground ,bright_red :weight bold))))
   `(escape-glyph        ((t (:foreground ,bright_aqua))))
   `(fringe              ((t (:foreground ,light3))))
   `(header-line         ((t (:background ,dark1 :foreground ,bright_yellow))))
   `(highlight           ((t (:background ,dark2))))
   `(lazy-highlight      ((t (:background ,bright_yellow :foreground ,dark0))))
   `(link                ((t (:foreground ,bright_blue :weight bold :underline t))))
   `(link-visited        ((t (:foreground ,bright_purple :weight bold :underline t))))
   `(match               ((t (:background ,bright_orange :foreground ,dark0))))
   `(menu                ((t (:background ,dark2 :foreground ,light1))))
   `(minibuffer-prompt   ((t (:foreground ,bright_aqua :weight bold))))
   `(mode-line           ((t (:background ,dark1 :foreground ,light1))))
   `(mode-line-buffer-id ((t (:foreground ,bright_blue :weight bold))))
   `(mode-line-highlight ((t (:box (:line-width -1 :color ,bright_gray :style released-button)))))
   `(mode-line-inactive  ((t (:background ,dark1 :foreground ,dark4))))
   '(region              ((t (:inverse-video t))))
   `(secondary-selection ((t (:background ,dark0_s))))
   `(shadow              ((t (:foreground ,neutral_gray))))
   `(success             ((t (:foreground ,bright_green :weight bold))))
   `(tooltip             ((t (:background ,dark2 :foreground ,light1))))
   `(trailing-whitespace ((t (:background ,bright_red))))
   `(vertical-border     ((t (:background ,dark1 :foreground ,dark4))))
   `(warning             ((t (:foreground ,bright_orange :weight bold))))

   ;; hl-line-mode
   `(hl-line      ((t (:background ,dark1)))) ; old Emacsen
   `(hl-line-face ((t (:background ,dark1))))

   ;; font lock
   `(font-lock-builtin-face              ((t (:foreground ,bright_orange))))
   `(font-lock-comment-delimiter-face    ((t (:foreground ,faded_green))))
   `(font-lock-comment-face              ((t (:foreground ,neutral_gray))))
   `(font-lock-constant-face             ((t (:foreground ,bright_purple))))
   `(font-lock-doc-face                  ((t (:foreground ,light3))))
   `(font-lock-function-name-face        ((t (:foreground ,bright_green :weight bold))))
   `(font-lock-keyword-face              ((t (:foreground ,bright_red))))
   `(font-lock-negation-char-face        ((t (:foreground ,bright_yellow :weight bold))))
   `(font-lock-preprocessor-face         ((t (:foreground ,bright_aqua))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,bright_aqua))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,bright_purple))))
   `(font-lock-string-face               ((t (:foreground ,bright_green))))
   `(font-lock-type-face                 ((t (:foreground ,bright_yellow))))
   `(font-lock-variable-name-face        ((t (:foreground ,bright_blue))))
   `(font-lock-warning-face              ((t (:foreground ,bright_red :weight bold))))

   `(c-nonbreakable-space-face           ((t (:background ,bright_red :foreground ,dark0))))

   ;; whitespace-mode
   `(whitespace-hspace      ((t (:foreground ,dark2))))
   `(whitespace-indentation ((t (:foreground ,dark2))))
   `(whitespace-newline     ((t (:foreground ,dark2))))
   `(whitespace-space       ((t (:foreground ,dark2))))
   `(whitespace-tab         ((t (:foreground ,dark2))))

   `(whitespace-empty
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_orange))))
   `(whitespace-space-after-tab
     ((((supports :underline (:style wave)))
       (:foreground ,dark2 :underline (:style wave :color ,bright_orange)))
      (t (:foreground ,dark2 :underline ,bright_orange))))
   `(whitespace-line
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_orange))))
   `(whitespace-space-before-tab
     ((((supports :underline (:style wave)))
       (:foreground ,dark2 :underline (:style wave :color ,bright_orange)))
      (t (:foreground ,dark2 :underline ,bright_orange))))
   `(whitespace-trailing
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_red))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face  ((t (:foreground ,bright_red))))
   `(rainbow-delimiters-depth-2-face  ((t (:foreground ,bright_green))))
   `(rainbow-delimiters-depth-3-face  ((t (:foreground ,bright_orange))))
   `(rainbow-delimiters-depth-4-face  ((t (:foreground ,bright_blue))))
   `(rainbow-delimiters-depth-5-face  ((t (:foreground ,bright_yellow))))
   `(rainbow-delimiters-depth-6-face  ((t (:foreground ,bright_purple))))
   `(rainbow-delimiters-depth-7-face  ((t (:foreground ,neutral_red))))
   `(rainbow-delimiters-depth-8-face  ((t (:foreground ,neutral_green))))
   `(rainbow-delimiters-depth-9-face  ((t (:foreground ,neutral_orange))))
   `(rainbow-delimiters-depth-10-face ((t (:foreground ,neutral_blue))))
   `(rainbow-delimiters-depth-11-face ((t (:foreground ,neutral_yellow))))
   `(rainbow-delimiters-depth-12-face ((t (:foreground ,neutral_purple))))
   `(rainbow-delimiters-unmatched-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_orange)))
      (t (:underline ,bright_orange))))

   ;; linum{,-relative}
   `(linum                       ((t (:foreground ,dark4))))
   `(linum-relative-current-face ((t (:background ,dark1 :foreground ,bright_yellow))))

   ;; compilation
   '(compilation-error                ((t (:inherit error))))
   '(compilation-warning              ((t (:inherit warning))))
   `(compilation-column-number        ((t (:foreground ,bright_aqua))))
   `(compilation-enter-directory-face ((t (:foreground ,bright_green))))
   `(compilation-info                 ((t (:foreground ,bright_blue :underline t))))
   `(compilation-leave-directory-face ((t (:foreground ,bright_blue))))
   `(compilation-line-number          ((t (:foreground ,bright_yellow))))
   `(compilation-mode-line-exit       ((t (:foreground ,bright_green :weight bold))))
   `(compilation-mode-line-fail       ((t (:foreground ,bright_red :weight bold))))
   `(compilation-mode-line-run        ((t (:foreground ,bright_orange :weight bold))))

   ;; highlight-indentation-mode
   `(highlight-indentation-current-column-face ((t (:background ,dark3))))
   `(highlight-indentation-face                ((t (:background ,dark2))))

   ;; show-paren-mode
   `(show-paren-match    ((t (:background ,dark1 :underline t))))
   `(show-paren-mismatch ((t (:background ,bright_red :foreground ,dark0))))

   ;; smartparens-mode
   '(sp-show-pair-match-face    ((t (:inherit show-paren-match))))
   '(sp-show-pair-mismatch-face ((t (:inherit show-paren-mismatch))))

   ;; completions
   '(completions-annotations      ((t (:inherit font-lock-doc-face))))
   '(completions-common-part      ((t (:inherit shadow))))
   `(completions-first-difference ((t (:foreground ,bright_yellow :weight bold))))

   ;; grep
   `(grep-context-face ((t (:foreground ,light1))))
   `(grep-error-face   ((t (:foreground ,bright_red :weight bold :underline t))))
   `(grep-hit-face     ((t (:foreground ,bright_blue))))
   `(grep-match-face   ((t (:foreground ,bright_orange :weight bold))))

   ;; isearch
   `(isearch      ((t (:background ,bright_orange :foreground ,dark0))))
   `(isearch-fail ((t (:background ,bright_red :foreground ,dark0))))

   ;; buffer-menu
   '(buffer-menu-buffer ((t (:inherit mode-line-buffer-id))))

   ;; ido-mode
   `(ido-first-match ((t (:foreground ,bright_yellow :weight bold))))
   `(ido-only-match  ((t (:foreground ,bright_orange :weight bold))))
   `(ido-subdir      ((t (:foreground ,neutral_blue :weight bold))))
   `(ido-indicator   ((t (:background ,bright_yellow :foreground ,dark0 :width condensed))))

   ;; message-mode
   `(message-cited-text        ((t (:foreground ,neutral_purple))))
   '(message-header-name       ((t (:inherit font-lock-comment-face))))
   '(message-separator         ((t (:inherit shadow))))
   `(message-header-cc         ((t (:foreground ,bright_blue :weight bold))))
   `(message-header-newsgroups ((t (:foreground ,bright_yellow :weight bold))))
   `(message-header-other      ((t (:foreground ,bright_aqua :weight bold))))
   `(message-header-subject    ((t (:foreground ,bright_orange :weight bold))))
   `(message-header-to         ((t (:foreground ,bright_green :weight bold))))
   `(message-header-xheader    ((t (:foreground ,bright_purple :weight bold))))
   `(message-mml               ((t (:foreground ,bright_yellow :weight bold))))

   ;; gnus
   '(gnus-header-content    ((t (:inherit message-header-other))))
   '(gnus-header-from       ((t (:inherit message-header-other))))
   '(gnus-header-name       ((t (:inherit message-header-name))))
   '(gnus-header-newsgroups ((t (:inherit message-header-newsgroups))))
   '(gnus-header-subject    ((t (:inherit message-header-subject))))

   `(gnus-emphasis-highlight-words ((t (:background ,dark0_h :foreground ,bright_purple))))

   `(gnus-server-agent   ((t (:foreground ,bright_aqua :weight bold))))
   `(gnus-server-closed  ((t (:foreground ,bright_blue :slant italic))))
   `(gnus-server-denied  ((t (:foreground ,bright_red :weight bold))))
   `(gnus-server-offline ((t (:foreground ,bright_yellow :weight bold))))
   `(gnus-server-opened  ((t (:foreground ,bright_green :weight bold))))

   `(gnus-summary-cancelled           ((t (:foreground ,bright_red :weight bold))))
   `(gnus-summary-high-ancient        ((t (:foreground ,bright_blue :weight bold))))
   `(gnus-summary-high-read           ((t (:foreground ,bright_green :weight bold))))
   `(gnus-summary-high-ticked         ((t (:foreground ,bright_orange :weight bold))))
   `(gnus-summary-high-undownloaded   ((t (:foreground ,light3 :weight bold))))
   `(gnus-summary-high-unread         ((t (:foreground ,light1 :weight bold))))
   `(gnus-summary-low-ancient         ((t (:foreground ,neutral_blue))))
   `(gnus-summary-low-read            ((t (:foreground ,neutral_green))))
   `(gnus-summary-low-ticked          ((t (:foreground ,neutral_orange))))
   `(gnus-summary-low-undownloaded    ((t (:foreground ,light4))))
   `(gnus-summary-low-unread          ((t (:foreground ,light2))))
   `(gnus-summary-normal-ancient      ((t (:foreground ,bright_blue))))
   `(gnus-summary-normal-read         ((t (:foreground ,bright_green))))
   `(gnus-summary-normal-ticked       ((t (:foreground ,bright_orange))))
   `(gnus-summary-normal-undownloaded ((t (:foreground ,light3))))
   `(gnus-summary-normal-unread       ((t (:foreground ,light1))))
   `(gnus-summary-selected            ((t (:foreground ,bright_yellow :weight bold))))

   '(gnus-cite-attribution ((t (:slant italic))))
   `(gnus-cite-1           ((t (:foreground ,bright_red))))
   `(gnus-cite-2           ((t (:foreground ,bright_green))))
   `(gnus-cite-3           ((t (:foreground ,bright_orange))))
   `(gnus-cite-4           ((t (:foreground ,bright_blue))))
   `(gnus-cite-5           ((t (:foreground ,bright_yellow))))
   `(gnus-cite-6           ((t (:foreground ,bright_purple))))
   `(gnus-cite-7           ((t (:foreground ,neutral_red))))
   `(gnus-cite-8           ((t (:foreground ,neutral_green))))
   `(gnus-cite-9           ((t (:foreground ,neutral_orange))))
   `(gnus-cite-10          ((t (:foreground ,neutral_blue))))
   `(gnus-cite-11          ((t (:foreground ,neutral_yellow))))

   `(gnus-group-news-1         ((t (:foreground ,bright_red :weight bold))))
   `(gnus-group-news-1-empty   ((t (:foreground ,neutral_red))))
   `(gnus-group-news-2         ((t (:foreground ,bright_orange :weight bold))))
   `(gnus-group-news-2-empty   ((t (:foreground ,neutral_orange))))
   `(gnus-group-news-3         ((t (:foreground ,bright_yellow :weight bold))))
   `(gnus-group-news-3-empty   ((t (:foreground ,neutral_yellow))))
   `(gnus-group-news-4         ((t (:foreground ,bright_green :weight bold))))
   `(gnus-group-news-4-empty   ((t (:foreground ,neutral_green))))
   `(gnus-group-news-5         ((t (:foreground ,bright_blue :weight bold))))
   `(gnus-group-news-5-empty   ((t (:foreground ,neutral_blue))))
   `(gnus-group-news-6         ((t (:foreground ,bright_purple :weight bold))))
   `(gnus-group-news-6-empty   ((t (:foreground ,neutral_purple))))
   `(gnus-group-news-low       ((t (:foreground ,light1))))
   `(gnus-group-news-low-empty ((t (:foreground ,light2))))

   '(gnus-group-mail-1         ((t (:inherit gnus-group-news-1))))
   '(gnus-group-mail-1-empty   ((t (:inherit gnus-group-news-1-empty))))
   '(gnus-group-mail-2         ((t (:inherit gnus-group-news-2))))
   '(gnus-group-mail-2-empty   ((t (:inherit gnus-group-news-2-empty))))
   '(gnus-group-mail-3         ((t (:inherit gnus-group-news-3))))
   '(gnus-group-mail-3-empty   ((t (:inherit gnus-group-news-3-empty))))
   '(gnus-group-mail-4         ((t (:inherit gnus-group-news-4))))
   '(gnus-group-mail-4-empty   ((t (:inherit gnus-group-news-4-empty))))
   '(gnus-group-mail-5         ((t (:inherit gnus-group-news-5))))
   '(gnus-group-mail-5-empty   ((t (:inherit gnus-group-news-5-empty))))
   '(gnus-group-mail-6         ((t (:inherit gnus-group-news-6))))
   '(gnus-group-mail-6-empty   ((t (:inherit gnus-group-news-6-empty))))
   '(gnus-group-mail-low       ((t (:inherit gnus-group-news-low-empty))))
   '(gnus-group-mail-low-empty ((t (:inherit gnus-group-news-low-empty))))

   `(gnus-signature ((t (:foreground ,bright_aqua))))

   `(mm-command-ouput ((t (:foreground ,bright_yellow))))
   `(mm-uu-extract    ((t (:background ,dark0_h :foreground ,bright_yellow))))

   ;; wid-edit
   '(widget-button            ((t (:weight bold))))
   `(widget-button-pressed    ((t (:foreground ,bright_purple :underline t))))
   `(widget-documentation     ((t (:foreground ,bright_green))))
   `(widget-field             ((t (:background ,dark2 :foreground ,light1))))
   '(widget-inactive          ((t (:inherit shadow))))
   '(widget-single-line-field ((t (:inherit widget-field))))

   ;; custom-mode
   `(custom-state                   ((t (:foreground ,bright_green))))
   `(custom-group-tag               ((t (:foreground ,bright_yellow :weight bold :height 1.2 :inherit variable-pitch))))
   `(custom-group-tag-1             ((t (:foreground ,bright_purple :inherit custom-group-tag))))
   `(custom-group-subtitle          ((t (:foreground ,bright_yellow :weight bold :height 1.05 :inherit variable-pitch))))
   `(custom-variable-tag            ((t (:foreground ,bright_aqua :weight bold :inherit variable-pitch))))
   `(custom-visibility              ((t (:foreground ,bright_orange :height 0.8))))
   `(custom-button                  ((t (:background ,light2 :foreground ,dark0
                                                     :box (:line-width 1 :style released-button)))))
   `(custom-button-mouse            ((t (:background ,light3 :foreground ,dark0
                                                     :box (:line-width 1 :style released-button)))))
   `(custom-button-pressed          ((t (:background ,light3 :foreground ,dark0
                                                     :box (:line-width 1 :style pressed-button)))))
   `(custom-button-pressed-unraised ((t (:foreground ,bright_purple :inherit custom-button-unraised))))
   '(custom-button-unraised         ((t (:inherit underline))))
   '(custom-comment                 ((t (:inherit widget-single-line-field))))
   '(custom-comment-tag             ((t (:inherit font-lock-comment-face))))
   `(custom-changed                 ((t (:background ,bright_blue :foreground ,dark0))))
   '(custom-themed                  ((t (:inherit custom-changed))))
   '(custom-modified                ((t (:inherit custom-changed))))
   `(custom-set                     ((t (:foreground ,bright_blue))))
   `(custom-invalid                 ((t (:background ,bright_red :foreground ,dark0))))
   `(custom-rogue                   ((t (:background ,bright_orange :foreground ,dark0))))

   ;; ediff
   `(ediff-current-diff-A        ((t (:background ,faded_red :foreground ,light1))))
   `(ediff-current-diff-Ancestor ((t (:background ,faded_red :foreground ,light1))))
   `(ediff-current-diff-B        ((t (:background ,faded_green :foreground ,light1))))
   `(ediff-current-diff-C        ((t (:background ,faded_aqua :foreground ,light1))))
   `(ediff-even-diff-A           ((t (:background ,dark0))))
   `(ediff-even-diff-Ancestor    ((t (:background ,dark0))))
   `(ediff-even-diff-B           ((t (:background ,dark0))))
   `(ediff-even-diff-C           ((t (:background ,dark0))))
   `(ediff-fine-diff-A           ((t (:background ,bright_red :foreground ,dark0 :weight bold))))
   `(ediff-fine-diff-Ancestor    ((t (:background ,bright_red :foreground ,dark0 :weight bold))))
   `(ediff-fine-diff-B           ((t (:background ,bright_green :foreground ,dark0 :weight bold))))
   `(ediff-fine-diff-C           ((t (:background ,bright_aqua :foreground ,dark0 :weight bold))))
   `(ediff-odd-diff-A            ((t (:background ,dark1))))
   `(ediff-odd-diff-Ancestor     ((t (:background ,dark1))))
   `(ediff-odd-diff-B            ((t (:background ,dark1))))
   `(ediff-odd-diff-C            ((t (:background ,dark1))))

   ;; diff
   `(diff-added          ((t (:background ,faded_green :foreground ,light1))))
   `(diff-changed        ((t (:background ,faded_aqua :foreground ,light1))))
   `(diff-file-header    ((t (:foreground ,bright_yellow))))
   `(diff-header         ((t (:background ,dark1))))
   `(diff-refine-added   ((t (:background ,bright_green :foreground ,dark0 :weight bold))))
   `(diff-refine-changed ((t (:background ,bright_aqua :foreground ,dark0 :weight bold))))
   `(diff-refine-removed ((t (:background ,bright_red :foreground ,dark0 :weight bold))))
   `(diff-removed        ((t (:background ,faded_red :foreground ,light1))))

   ;; anzu
   `(anzu-match-1    ((t (:background ,bright_green :foreground ,dark0))))
   `(anzu-match-2    ((t (:background ,bright_yellow :foreground ,dark0))))
   `(anzu-match-3    ((t (:background ,bright_aqua :foreground ,dark0))))
   `(anzu-mode-line  ((t (:foreground ,bright_purple :weight bold))))
   `(anzu-replace-to ((t (:foreground ,bright_yellow))))

   ;; company-mode
   `(company-echo-common                  ((t (:foreground ,bright_green))))
   `(company-tooltip                      ((t (:background ,dark1 :foreground ,light1))))
   `(company-tooltip-annotation           ((t (:background ,dark1 :foreground ,bright_orange))))
   '(company-tooltip-annotation-selection ((t (:inverse-video t :inherit company-tooltip-annotation))))
   '(company-tooltip-selection            ((t (:inverse-video t :inherit company-tooltip))))
   `(company-tooltip-mouse                ((t (:background ,dark2 :foreground ,light1))))
   `(company-tooltip-common               ((t (:foreground ,bright_green))))
   '(company-tooltip-common-selection     ((t (:inverse-video t :inherit company-tooltip-common))))
   `(company-scrollbar-fg                 ((t (:background ,light1))))
   `(company-scrollbar-bg                 ((t (:background ,light3))))
   `(company-template-field               ((t (:background ,bright_yellow :foreground ,dark0))))
   `(company-preview                      ((t (:foreground ,bright_green))))
   '(company-preview-common               ((t (:inverse-video t :inherit company-preview))))
   `(company-preview-search               ((t (:background ,bright_aqua :foreground ,dark0))))

   ;; AUCTeX
   `(font-latex-bold-face                 ((t (:foreground ,neutral_green :inherit bold))))
   `(font-latex-doctex-documentation-face ((t (:background ,dark0_h))))
   `(font-latex-italic-face               ((t (:foreground ,neutral_green :inherit italic))))
   `(font-latex-math-face                 ((t (:foreground ,bright_yellow))))
   `(font-latex-sectioning-5-face         ((t (:foreground ,bright_yellow :weight bold :inherit variable-pitch))))
   `(font-latex-sedate-face               ((t (:foreground ,bright_aqua))))
   '(font-latex-slide-title-face          ((t (:height 1.2 :inherit font-latex-sectioning-5-face))))
   `(font-latex-string-face               ((t (:foreground ,neutral_purple))))
   '(font-latex-verbatim-face             ((t (:inherit font-lock-constant-face))))
   '(font-latex-warning-face              ((t (:inherit font-lock-warning-face))))

   ;; markdown-mode
   `(markdown-blockquote-face       ((t (:foreground ,neutral_purple))))
   `(markdown-bold-face             ((t (:foreground ,neutral_green :inherit bold))))
   `(markdown-italic-face           ((t (:foreground ,neutral_green :inherit italic))))
   '(markdown-header-delimiter-face ((t (:inherit font-lock-comment-delimiter-face))))
   '(markdown-header-face           ((t (:inherit markdown-header-face-1))))
   `(markdown-header-face-1         ((t (:foreground ,bright_red :weight bold))))
   `(markdown-header-face-2         ((t (:foreground ,bright_green :weight bold))))
   `(markdown-header-face-3         ((t (:foreground ,bright_orange :weight bold))))
   `(markdown-header-face-4         ((t (:foreground ,bright_blue :weight bold))))
   `(markdown-header-face-5         ((t (:foreground ,bright_yellow :weight bold))))
   `(markdown-header-face-6         ((t (:foreground ,bright_purple :weight bold))))
   '(markdown-header-rule-face      ((t (:inherit markdown-header-delimiter-face))))
   `(markdown-link-face             ((t (:foreground ,bright_yellow))))
   `(markdown-markup-face           ((t (:foreground ,light2))))
   `(markdown-math-face             ((t (:foreground ,bright_yellow))))
   `(markdown-strike-through-face   ((t (:foreground ,neutral_green :strike-through t))))
   '(markdown-url-face              ((t (:inherit link))))

   ;; flyspell-mode
   `(flyspell-duplicate
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_orange)))
      (t (:underline ,bright_orange))))
   `(flyspell-incorrect
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_red))))

   ;; flymake-mode
   `(flymake-errline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_red))))
   `(flymake-warnline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_orange)))
      (t (:underline ,bright_orange))))
   `(flymake-infoline
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_green)))
      (t (:underline ,bright_green))))

   ;; flycheck-mode
   `(flycheck-error
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_red))))
   `(flycheck-warning
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_orange)))
      (t (:underline ,bright_orange))))
   `(flycheck-info
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_green)))
      (t (:underline ,bright_green))))

   '(flycheck-fringe-error   ((t (:inherit error))))
   '(flycheck-fringe-warning ((t (:inherit warning))))
   '(flycheck-fringe-info    ((t (:inherit success))))

   '(flycheck-error-list-line-number   ((t (:inherit compilation-line-number))))
   '(flycheck-error-list-column-number ((t (:inherit compilation-column-number))))

   ;; comint
   '(comint-highlight-input  ((t (:weight bold))))
   `(comint-highlight-prompt ((t (:foreground ,bright_aqua :weight bold))))

   ;; SLIME
   '(slime-repl-input-face            ((t (:weight bold))))
   `(slime-repl-inputed-output-face   ((t (:foreground ,bright_red))))
   `(slime-repl-output-face           ((t (:foreground ,bright_green))))
   `(slime-repl-output-mouseover-face ((t (:box (:line-width -1 :color ,bright_gray :style released-button)
                                                :inherit slime-repl-inputed-output-face))))
   `(slime-repl-prompt-face           ((t (:foreground ,bright_aqua :weight bold))))
   `(slime-repl-result-face           ((t (:foreground ,bright_purple))))

   `(slime-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_red))))
   `(slime-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_orange)))
      (t (:underline ,bright_orange))))
   `(slime-style-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_yellow)))
      (t (:underline ,bright_yellow))))
   `(slime-note-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_green)))
      (t (:underline ,bright_green))))

   `(sldb-frame-label-face            ((t (:foreground ,dark4))))
   `(sldb-restartable-frame-line-face ((t (:foreground ,bright_green))))
   '(sldb-topline-face                ((t (:weight bold))))

   ;; avy
   '(avy-background-face      ((t (:inherit shadow))))
   '(avy-goto-char-timer-face ((t (:inherit highlight))))
   `(avy-lead-face            ((t (:background ,bright_red :foreground ,dark0 :weight bold))))
   `(avy-lead-face-0          ((t (:background ,bright_yellow :foreground ,dark0 :weight bold))))
   `(avy-lead-face-1          ((t (:background ,bright_aqua :foreground ,dark0 :weight bold))))
   `(avy-lead-face-2          ((t (:background ,bright_purple :foreground ,dark0 :weight bold))))

   ;; ESS
   `(ess-bp-fringe-browser-face          ((t (:foreground ,neutral_blue))))
   `(ess-bp-fringe-inactive-face         ((t (:foreground ,dark4))))
   `(ess-bp-fringe-logger-face           ((t (:foreground ,bright_orange))))
   `(ess-bp-fringe-recover-face          ((t (:foreground ,bright_purple))))
   `(ess-debug-blink-ref-not-found-face  ((t (:background ,bright_red :foreground ,dark0))))
   `(ess-debug-blink-same-ref-face       ((t (:background ,bright_blue :foreground ,dark0))))
   '(ess-debug-current-debug-line-face   ((t (:inherit highlight))))
   `(ess-tracebug-last-input-fringe-face ((t (:foreground ,neutral_blue :overline ,neutral_blue))))
   '(ess-watch-current-block-face        ((t (:inherit highlight))))

   ;; undo-tree-mode
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,light1 :weight bold))))
   `(undo-tree-visualizer-current-face       ((t (:foreground ,bright_red))))
   `(undo-tree-visualizer-default-face       ((t (:foreground ,dark3))))
   `(undo-tree-visualizer-register-face      ((t (:foreground ,bright_yellow))))
   `(undo-tree-visualizer-unmodified-face    ((t (:foreground ,bright_aqua))))

   ;; dired{,+}
   `(dired-directory  ((t (:foreground ,neutral_blue :weight bold))))
   `(dired-flagged    ((t (:foreground ,bright_red :weight bold :underline t))))
   `(dired-header     ((t (:background ,dark1 :foreground ,bright_yellow))))
   '(dired-ignored    ((t (:inherit shadow))))
   `(dired-mark       ((t (:foreground ,bright_green :weight bold))))
   `(dired-marked     ((t (:foreground ,bright_purple :weight bold))))
   `(dired-perm-write ((t (:foreground ,bright_yellow))))
   `(dired-symlink    ((t (:foreground ,neutral_aqua :slant italic))))
   `(dired-warning    ((t (:foreground ,bright_orange :underline t))))

   `(diredp-autofile-name          ((t (:foreground ,bright_orange))))
   `(diredp-compressed-file-name   ((t (:foreground ,neutral_red :weight bold))))
   '(diredp-compressed-file-suffix ((t (:inherit diredp-compressed-file-name))))
   `(diredp-date-time              ((t (:foreground ,bright_yellow))))
   `(diredp-deletion               ((t (:foreground ,bright_red :weight bold :underline t))))
   `(diredp-deletion-file-name     ((t (:foreground ,bright_red :weight bold :underline t))))
   `(diredp-dir-heading            ((t (:background ,dark1 :foreground ,bright_yellow))))
   `(diredp-dir-name               ((t (:foreground ,neutral_blue :weight bold))))
   `(diredp-dir-priv               ((t (:foreground ,neutral_blue))))
   `(diredp-exec-priv              ((t (:foreground ,neutral_green))))
   `(diredp-executable-tag         ((t (:foreground ,neutral_green))))
   `(diredp-file-name              ((t (:foreground ,light1))))
   `(diredp-file-suffix            ((t (:foreground ,bright_aqua))))
   `(diredp-flag-mark              ((t (:background ,dark1 :foreground ,bright_purple))))
   `(diredp-flag-mark-line         ((t (:background ,dark1))))
   '(diredp-ignored-file-name      ((t (:inherit shadow))))
   `(diredp-link-priv              ((t (:foreground ,neutral_aqua))))
   `(diredp-mode-line-flagged      ((t (:foreground ,bright_red :weight bold))))
   `(diredp-mode-line-marked       ((t (:foreground ,bright_purple :weight bold))))
   `(diredp-no-priv                ((t (:foreground ,light1))))
   `(diredp-number                 ((t (:foreground ,bright_purple))))
   `(diredp-other-priv             ((t (:foreground ,light1))))
   `(diredp-rare-priv              ((t (:foreground ,neutral_red))))
   `(diredp-read-priv              ((t (:foreground ,neutral_purple))))
   `(diredp-symlink                ((t (:foreground ,neutral_aqua :slant italic))))
   `(diredp-tagged-autofile-name   ((t (:background ,dark0_h :foreground ,bright_orange))))
   `(diredp-write-priv             ((t (:foreground ,neutral_orange))))

   ;; ert
   '(ert-test-result-expected   ((t (:inherit success))))
   '(ert-test-result-unexpected ((t (:inherit error))))

   ;; eshell
   `(eshell-ls-archive          ((t (:foreground ,neutral_red :weight bold))))
   `(eshell-ls-backup           ((t (:foreground ,neutral_gray))))
   `(eshell-ls-clutter          ((t (:foreground ,dark3))))
   `(eshell-ls-directory        ((t (:foreground ,neutral_blue :weight bold))))
   `(eshell-ls-executable       ((t (:foreground ,neutral_green :weight bold))))
   `(eshell-ls-missing          ((t (:foreground ,neutral_orange :weight bold))))
   `(eshell-ls-product          ((t (:foreground ,light4))))
   `(eshell-ls-readonly         ((t (:foreground ,neutral_purple :weight bold))))
   `(eshell-ls-special          ((t (:background ,dark0_s :foreground ,bright_purple :weight bold))))
   `(eshell-ls-symlink          ((t (:foreground ,neutral_aqua :weight bold))))
   `(eshell-ls-unreadable       ((t (:foreground ,neutral_yellow))))
   `(eshell-prompt              ((t (:foreground ,bright_aqua :weight bold))))

   ;; {ansi-,}term
   `(term-color-black    ((t (:background ,dark0 :foreground ,dark0))))
   `(term-color-blue     ((t (:background ,neutral_green :foreground ,neutral_blue))))
   `(term-color-cyan     ((t (:background ,neutral_aqua :foreground ,neutral_aqua))))
   `(term-color-green    ((t (:background ,neutral_green :foreground ,neutral_green))))
   `(term-color-magenta  ((t (:background ,neutral_purple :foreground ,neutral_purple))))
   `(term-color-red      ((t (:background ,neutral_red :foreground ,neutral_red))))
   `(term-color-white    ((t (:background ,light1 :foreground ,light1))))
   `(term-color-yellow   ((t (:background ,neutral_yellow :foreground ,neutral_yellow))))

   ;; which-func-mode
   `(which-func ((t (:foreground ,bright_green :weight bold))))

   ;; info
   `(info-menu-star ((t (:foreground ,bright_red))))
   `(info-node      ((t (:foreground ,bright_orange :weight bold :slant italic))))
   `(info-title-4   ((t (:foreground ,bright_yellow :weight bold :inherit variable-pitch))))

   ;; outline-mode
   `(outline-1 ((t (:foreground ,bright_red :weight bold))))
   `(outline-2 ((t (:foreground ,bright_green :weight bold))))
   `(outline-3 ((t (:foreground ,bright_orange :weight bold))))
   `(outline-4 ((t (:foreground ,bright_blue :weight bold))))
   `(outline-5 ((t (:foreground ,bright_yellow :weight bold))))
   `(outline-6 ((t (:foreground ,bright_purple :weight bold))))
   `(outline-7 ((t (:foreground ,bright_aqua :weight bold))))
   `(outline-8 ((t (:foreground ,neutral_orange :weight bold))))

   ;; ivy
   `(ivy-current-match           ((t (:background ,neutral_orange :foreground ,dark0 :weight bold))))
   `(ivy-minibuffer-match-face-1 ((t (:background ,bright_orange :foreground ,dark0 :weight bold))))
   `(ivy-minibuffer-match-face-2 ((t (:background ,bright_purple :foreground ,dark0 :weight bold))))
   `(ivy-minibuffer-match-face-3 ((t (:background ,bright_aqua :foreground ,dark0 :weight bold))))
   `(ivy-minibuffer-match-face-4 ((t (:background ,bright_blue :foreground ,dark0 :weight bold))))
   `(ivy-remote                  ((t (:foreground ,neutral_purple))))
   `(ivy-modified-buffer         ((t (:foreground ,neutral_red))))

   ;; swiper
   '(swiper-line-face    ((t (:inherit hl-line))))
   '(swiper-match-face-1 ((t (:inherit ivy-minibuffer-match-face-1))))
   '(swiper-match-face-2 ((t (:inherit ivy-minibuffer-match-face-2))))
   '(swiper-match-face-3 ((t (:inherit ivy-minibuffer-match-face-3))))
   '(swiper-match-face-4 ((t (:inherit ivy-minibuffer-match-face-4))))

   ;; haskell-mode
   '(haskell-debug-muted-face        ((t (:inherit shadow))))
   '(haskell-debug-trace-number-face ((t (:inherit compilation-line-number))))
   `(haskell-debug-newline-face      ((t (:foreground ,bright_purple :underline t))))

   `(haskell-error-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_red)))
      (t (:underline ,bright_red))))
   `(haskell-hole-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_blue)))
      (t (:underline ,bright_blue))))
   `(haskell-warning-face
     ((((supports :underline (:style wave)))
       (:underline (:style wave :color ,bright_orange)))
      (t (:underline ,bright_orange))))

   `(haskell-interactive-face-prompt ((t (:foreground ,bright_aqua :weight bold))))

   ;; elfeed
   `(elfeed-search-date-face         ((t (:foreground ,bright_yellow))))
   `(elfeed-search-feed-face         ((t (:foreground ,bright_purple))))
   `(elfeed-search-filter-face       ((t (:foreground ,bright_blue))))
   `(elfeed-search-last-update-face  ((t (:foreground ,bright_yellow))))
   `(elfeed-search-tag-face          ((t (:foreground ,bright_aqua))))
   `(elfeed-search-title-face        ((t (:foreground ,light3))))
   `(elfeed-search-unread-count-face ((t (:foreground ,light1 :weight bold))))
   `(elfeed-search-unread-title-face ((t (:foreground ,light1 :weight bold))))

   `(elfeed-log-date-face        ((t (:foreground ,bright_yellow))))
   `(elfeed-log-error-level-face ((t :foreground ,bright_red)))
   `(elfeed-log-warn-level-face  ((t (:foreground ,bright_orange))))
   `(elfeed-log-info-level-face  ((t (:foreground ,bright_green))))

   ;; which-key-mode
   `(which-key-command-description-face ((t (:foreground ,bright_green))))
   `(which-key-group-description-face   ((t (:foreground ,bright_orange))))
   `(which-key-highlighted-command-face ((t (:background ,dark1 :foreground ,bright_green))))
   `(which-key-key-face                 ((t (:foreground ,bright_purple))))
   '(which-key-note-face                ((t (:inherit font-lock-doc-face))))
   '(which-key-separator-face           ((t (:inherit shadow))))
   `(which-key-special-key-face         ((t (:foreground ,bright_yellow))))

   ;; erc
   '(erc-action-face            ((t (:slant italic))))
   '(erc-bold-face              ((t (:weight bold))))
   '(erc-button                 ((t (:underline t))))
   '(erc-command-indicator-face ((t (:weight bold))))
   `(erc-current-nick-face      ((t (:foreground ,bright_yellow :weight bold))))
   `(erc-dangerous-host-face    ((t (:foreground ,bright_red :weight bold))))
   '(erc-default-face           ((t (:inherit default))))
   '(erc-direct-msg-face        ((t (:inherit erc-default-face))))
   `(erc-error-face             ((t (:foreground ,bright_red :weight bold))))
   '(erc-fool-face              ((t (:inherit shadow))))
   '(erc-header-line            ((t (:inherit header-line))))
   `(erc-input-face             ((t (:foreground ,bright_aqua))))
   '(erc-inverse-face           ((t (:inverse-video t))))
   `(erc-keyword-face           ((t (:foreground ,bright_yellow :weight bold))))
   '(erc-my-nick-face           ((t (:inherit erc-current-nick-face))))
   '(erc-my-nick-prefix-face    ((t (:inherit erc-nick-prefix-face))))
   `(erc-nick-default-face      ((t (:foreground ,bright_blue :weight bold))))
   `(erc-nick-msg-face          ((t (:foreground ,bright_orange :weight bold))))
   '(erc-nick-prefix-face       ((t (:weight bold :inherit erc-default-face))))
   `(erc-notice-face            ((t (:foreground ,bright_purple))))
   `(erc-pal-face               ((t (:foreground ,bright_green))))
   `(erc-prompt-face            ((t (:foreground ,bright_aqua :weight bold))))
   `(erc-timestamp-face         ((t (:foreground ,bright_yellow))))
   '(erc-underline-face         ((t (:underline t)))))

  (custom-theme-set-variables
   'gruvbox-dark

   ;; frame-background
   '(frame-background-mode 'dark)

   ;; fci-mode
   `(fci-rule-color ,neutral_blue)

   ;; ansi-colors
   `(ansi-color-names-vector [,dark0 ,bright_red ,bright_green ,bright_yellow
                                     ,bright_blue ,bright_purple ,bright_aqua ,light1])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'gruvbox-dark)

;;; gruvbox-dark-theme.el ends here
