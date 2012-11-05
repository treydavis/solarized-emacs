;;; solarized.el --- Solarized for Emacs.

;; Copyright (C) 2011,2012 Bozhidar Batsov

;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; Author: Thomas Frössman <thomasf@jossystem.se>
;; URL: http://github.com/bbatsov/solarized-emacs
;; Version: 0.5.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; A port of Solarized to Emacs.
;;
;;; Installation:
;;
;;   Drop the `solarized-theme.el` somewhere in your `load-path` and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;; Don't forget that the theme requires Emacs 24.
;;
;;; Bugs
;;
;; None that I'm aware of.
;;
;;; Credits
;;
;; Ethan Schoonover created the original theme for vim on such this port
;; is based.
;;
;;; Code



;; experimental branch supporting terminals with different capabilites at once (emacsclient)
;; - docs
;; - organize everything
;; - remove old stuff
;; - child themes
;; - theme custom vars
;; - hc/lc colors



(defcustom solarized-broken-srgb (if (and (eq system-type 'darwin)
                                          (eq window-system 'ns))
                                     t
                                   nil)
  "Emacs bug #8402 results in incorrect color handling on Macs. If this is t
\(the default on Macs), Solarized works around it with alternative colors.
However, these colors are not totally portable, so you may be able to edit
the \"Gen RGB\" column in solarized-definitions.el to improve them further."
  :type 'boolean
  :group 'solarized)

;; FIXME: The Generic RGB colors will actually vary from device to device, but
;;        hopefully these are closer to the intended colors than the sRGB values
;;        that Emacs seems to dislike
(defvar solarized-colors           ; ANSI(Solarized terminal)
  ;; name     sRGB      Gen RGB   256       16              8
  '((base03  "#002b36" "#042028" "#1c1c1c" "brightblack"   "black")
    (base02  "#073642" "#0a2832" "#262626" "black"         "black")
    (base01  "#586e75" "#465a61" "#585858" "brightgreen"   "green")
    (base00  "#657b83" "#52676f" "#626262" "brightyellow"  "yellow")
    (base0   "#839496" "#708183" "#808080" "brightblue"    "blue")
    (base1   "#93a1a1" "#81908f" "#8a8a8a" "brightcyan"    "cyan")
    (base2   "#eee8d5" "#e9e2cb" "#e4e4e4" "white"         "white")
    (base3   "#fdf6e3" "#fcf4dc" "#ffffd7" "brightwhite"   "white")
    (yellow  "#b58900" "#a57705" "#af8700" "yellow"        "yellow")
    (orange  "#cb4b16" "#bd3612" "#d75f00" "brightred"     "red")
    (red     "#dc322f" "#c60007" "#d70000" "red"           "red")
    (magenta "#d33682" "#c61b6e" "#af005f" "magenta"       "magenta")
    (violet  "#6c71c4" "#5859b7" "#5f5faf" "brightmagenta" "magenta")
    (blue    "#268bd2" "#2075c7" "#0087ff" "blue"          "blue")
    (cyan    "#2aa198" "#259185" "#00afaf" "cyan"          "cyan")
    (green   "#859900" "#728a05" "#5f8700" "green"         "green")
    (yellow-lc  "red" "#7B6000" "red" "red" "red")
    (yellow-hc "red" "#DEB542" "red" "red" "red")
    (orange-lc  "red" "#8B2C02" "red" "red" "red")
    (orange-hc "red" "#F2804F" "red" "red" "red")
    (red-lc     "red" "#990A1B" "red" "red" "red")
    (red-hc    "red" "#FF6E64" "red" "red" "red")
    (magenta-lc "red" "#93115C" "red" "red" "red")
    (magenta-hc "red" "#F771AC" "red" "red" "red")
    (violet-lc  "red" "#3F4D91" "red" "red" "red")
    (violet-hc "red" "#9EA0E5" "red" "red" "red")
    (blue-lc    "red" "#00629D" "red" "red" "red")
    (blue-hc   "red" "#69B7F0" "red" "red" "red")
    (cyan-lc    "red" "#00736F" "red" "red" "red")
    (cyan-hc   "red" "#69CABF" "red" "red" "red")
    (green-lc   "red" "#546E00" "red" "red" "red")
    (green-hc  "red"  "#B4C342" "red" "red" "red")

    )
  "This is a table of all the colors used by the Solarized color theme. Each
   column is a different set, one of which will be chosen based on term
   capabilities, etc.")




(defun solarized-face-for-index (facespec index)
  "Creates a face from facespec where the colors use the names of
  the `solarized-colors'."
  (let ((new-fontspec (copy-list facespec)))
    (dolist (property '(:foreground :background :color))
      (when (plist-get new-fontspec property)
       (plist-put new-fontspec property
                  (nth index (assoc (plist-get new-fontspec property)
                                    solarized-colors)))))
    (when (plist-get new-fontspec :box)
      (plist-put new-fontspec :box (solarized-face-for-index
                                    (plist-get new-fontspec :box) index)))
    new-fontspec))

(defun solarized-flip (facespec)
  "Convert a facespec to its lightened or darkened counterpart"
  (let* ((reversing-alist '((base03 . base3) (base02 . base2) (base01 . base1)
                            (base00 . base0) (base0 . base00) (base1 . base01)
                            (base2 . base02) (base3 . base03)
                            (yellow-hc . yellow-lc)
                            (orange-hc . orange-lc)
                            (red-hc . red-lc)
                            (magenta-hc . magenta-lc)
                            (violet-hc . violet-lc)
                            (blue-hc . blue-lc)
                            (cyan-hc . cyan-lc)
                            (green-hc . green-lc)
                            (yellow-lc . yellow-hc)
                            (orange-lc . orange-hc)
                            (red-lc . red-hc)
                            (magenta-lc . magenta-hc)
                            (violet-lc . violet-hc)
                            (blue-lc . blue-hc)
                            (cyan-lc . cyan-hc)
                            (green-lc . green-hc))))

    (mapcar (lambda (term) (cond ((listp term) (solarized-flip term))
                            ((assoc term reversing-alist)
                             (cdr (assoc term reversing-alist)))
                            (t term))) facespec)))

(defun solarized-faces (facespecs mode)
  (mapcar (lambda (facespec-with-name)
            (let* ((name (car facespec-with-name))
                   (facespec (funcall
                              (if (eq mode 'dark) 'solarized-flip 'identity)
                              (second facespec-with-name)))
                   (flipped-facespec (solarized-flip facespec))
                   (facespec-tty-256 (solarized-face-for-index facespec 3))
                   (facespec-tty-term16 (solarized-face-for-index facespec 4))
                   (facespec-tty-term8 (solarized-face-for-index facespec 5))
                   (facespec-default (solarized-face-for-index facespec
                                                               (if solarized-broken-srgb 2 1))))
              `(,name
                ((((min-colors 257)) ,facespec-default)
                 (((min-colors 256)) ,facespec-tty-256)
                 (((min-colors 16)) ,facespec-tty-term16)
                 (((min-colors 8)) ,facespec-tty-term8)
                 ;; We should rarely if ever fall to the default.  If
                 ;; so, let's set it to the default light spec and
                 ;; hope for the best.
                 (t ,facespec-default))))) facespecs))

(defun solarized-color-definitions (mode)
  (list
   (append
    (solarized-faces
     `(
       (button  (:underline t))

       ;; basic coloring
       (default  (:foreground base00 :background base3))
       (shadow  (:foreground base1))
       (match  (:background base2 :foreground base01 :weight bold))
       (cursor  (:foreground base3 :background base00 :inverse-video t))
       (escape-glyph-face  (:foreground red))
       (fringe  (:foreground base00 :background base2))
       (header-line  (:foreground yellow
                                  :background base2
                                  :box (:line-width -1 :style released-button)))
       (highlight  (:background base2))
       (link  (:foreground yellow :underline t :weight bold))
       (link-visited  (:foreground yellow :underline t :weight normal))
       (success  (:foreground green ))
       (warning  (:foreground yellow ))
       (error  (:foreground orange))
       (lazy-highlight  (:foreground base01 :background base2 :bold t))
       (escape-glyph  (:foreground violet))

       ;; compilation
       (compilation-column-face  (:foreground yellow))
       (compilation-enter-directory-face  (:foreground green))
       (compilation-error-face  (:foreground red :weight bold :underline t))
       (compilation-face  (:foreground base00))
       (compilation-info-face  (:foreground blue))
       (compilation-info  (:foreground green :underline t))
       (compilation-leave-directory-face  (:foreground green))
       (compilation-line-face  (:foreground yellow))
       (compilation-line-number  (:foreground yellow))
       (compilation-message-face  (:foreground blue))
       (compilation-warning-face  (:foreground yellow :weight bold :underline t))

       ;; cua
       (cua-global-mark  (:background yellow :foreground base3))
       (cua-rectangle  (:inherit region :background magenta :foreground base3))
       (cua-rectangle-noselect  (:inherit region :background base2
                                          :foreground base1))

       ;; diary
       (diary  (:foreground yellow))

       ;; dired
       (dired-directory  (:foreground blue :weight normal))
       (dired-flagged  (:foreground red))
       (dired-header  (:foreground base3 :background blue))
       (dired-ignored  (:inherit shadow))
       (dired-mark  (:foreground yellow :weight bold))
       (dired-marked  (:foreground magenta :weight bold))
       (dired-perm-write  (:foreground base00 :underline t))
       (dired-symlink  (:foreground cyan :weight normal :slant italic))
       (dired-warning  (:foreground orange :underline t))

       ;; grep
       (grep-context-face  (:foreground base00))
       (grep-error-face  (:foreground red :weight bold :underline t))
       (grep-hit-face  (:foreground blue))
       (grep-match-face  (:foreground orange :weight bold))

       ;; faces used by isearch
       (isearch  (:foreground yellow :background base2 :bold t))
       (isearch-fail  (:foreground red :background base3 :bold t))

       ;; misc faces
       (menu  (:foreground base00 :background base3))
       (minibuffer-prompt  (:foreground base01))
       (mode-line
        (:foreground base00
                     :background base2
                     :box (:line-width -1 :style released-button)))
       (mode-line-buffer-id  (:foreground base01 :weight bold))
       (mode-line-inactive
        (:foreground base00
                     :background base3
                     :box (:line-width -1 :style released-button)))
       (region  (:foreground base3 :background base01))
       (secondary-selection  (:background base3))
       (trailing-whitespace  (:background red))
       (vertical-border  (:foreground base00))

       ;; font lock
       (font-lock-builtin-face  (:foreground blue :slant italic))
       (font-lock-comment-delimiter-face  (:foreground base1))
       (font-lock-comment-face  (:foreground base1))
       (font-lock-constant-face  (:foreground blue :weight bold))
       (font-lock-doc-face  (:foreground cyan :slant italic))
       (font-lock-doc-string-face  (:foreground blue))
       (font-lock-function-name-face  (:foreground blue))
       (font-lock-keyword-face  (:foreground green :weight bold))
       (font-lock-negation-char-face  (:foreground base00))
       (font-lock-preprocessor-face  (:foreground blue))
       (font-lock-string-face  (:foreground cyan))
       (font-lock-type-face  (:foreground yellow))
       (font-lock-variable-name-face  (:foreground blue))
       (font-lock-warning-face  (:foreground orange :weight bold :underline t))

       (c-annotation-face  (:inherit font-lock-constant-face))

       ;; external

       ;; ace-jump-mode
       (ace-jump-face-background
        (:foreground base1 :background base3 :inverse-video nil))
       (ace-jump-face-foreground
        (:foreground red :background base3 :inverse-video nil))

       ;; auto highlight symbol
       (ahs-definition-face  (:foreground base3 :background blue :underline t))
       (ahs-edit-mode-face  (:foreground base3 :background yellow))
       (ahs-face  (:foreground base3 :background blue))
       (ahs-plugin-bod-face  (:foreground base3 :background blue))
       (ahs-plugin-defalt-face  (:foreground base3 :background cyan))
       (ahs-plugin-whole-buffer-face  (:foreground base3 :background green))
       (ahs-warning-face  (:foreground red :weight bold))

       ;; bm
       (bm-face  (:background yellow-lc :foreground base3))
       (bm-fringe-face  (:background yellow-lc :foreground base3))
       (bm-fringe-persistent-face  (:background green-lc :foreground base3))
       (bm-persistent-face  (:background green-lc :foreground base3))

       ;; custom
       (custom-variable-tag  (:foreground cyan))
       (custom-comment-tag  (:foreground base1))
       (custom-group-tag  (:foreground blue))
       (custom-state  (:foreground green))

       ;; diff
       (diff-added  (:foreground green))
       (diff-changed  (:foreground yellow))
       (diff-removed  (:foreground red))
       (diff-header  (:background base3))
       (diff-file-header
        (:background base3 :foreground base00 :weight bold))

       ;; eshell
       (eshell-prompt  (:foreground yellow :weight bold))
       (eshell-ls-archive  (:foreground red :weight bold))
       (eshell-ls-backup  (:inherit font-lock-comment))
       (eshell-ls-clutter  (:inherit font-lock-comment))
       (eshell-ls-directory  (:foreground blue :weight bold))
       (eshell-ls-executable  (:foreground red :weight bold))
       (eshell-ls-unreadable  (:foreground base00))
       (eshell-ls-missing  (:inherit font-lock-warning))
       (eshell-ls-product  (:inherit font-lock-doc))
       (eshell-ls-special  (:foreground yellow :weight bold))
       (eshell-ls-symlink  (:foreground cyan :weight bold))

       ;; flymake
       (flymake-errline
        (:foreground red-hc :background red-lc :weight bold :underline t))
       (flymake-infoline  (:foreground green-hc :background green-lc))
       (flymake-warnline
        (:foreground yellow-hc :background yellow-lc :weight bold :underline t))

       ;; flyspell
       (flyspell-duplicate  (:foreground yellow :weight bold :underline t))
       (flyspell-incorrect  (:foreground red :weight bold :underline t))

       ;; erc
       (erc-action-face  (:inherit erc-default-face))
       (erc-bold-face  (:weight bold))
       (erc-current-nick-face  (:foreground blue :weight bold))
       (erc-dangerous-host-face  (:inherit font-lock-warning))
       (erc-default-face  (:foreground base00))
       (erc-direct-msg-face  (:inherit erc-default))
       (erc-error-face  (:inherit font-lock-warning))
       (erc-fool-face  (:inherit erc-default))
       (erc-highlight-face  (:inherit hover-highlight))
       (erc-input-face  (:foreground yellow))
       (erc-keyword-face  (:foreground blue :weight bold))
       (erc-nick-default-face  (:foreground yellow :weight bold))
       (erc-my-nick-face  (:foreground red :weight bold))
       (erc-nick-msg-face  (:inherit erc-default))
       (erc-notice-face  (:foreground green))
       (erc-pal-face  (:foreground orange :weight bold))
       (erc-prompt-face  (:foreground orange :background base3 :weight bold))
       (erc-timestamp-face  (:foreground green))
       (erc-underline-face  (:underline t))

       ;; gnus
       (gnus-group-mail-1-face  (:weight bold :inherit gnus-group-mail-1-empty))
       (gnus-group-mail-1-empty-face  (:inherit gnus-group-news-1-empty))
       (gnus-group-mail-2-face  (:weight bold :inherit gnus-group-mail-2-empty))
       (gnus-group-mail-2-empty-face  (:inherit gnus-group-news-2-empty))
       (gnus-group-mail-3-face  (:weight bold :inherit gnus-group-mail-3-empty))
       (gnus-group-mail-3-empty-face  (:inherit gnus-group-news-3-empty))
       (gnus-group-mail-4-face  (:weight bold :inherit gnus-group-mail-4-empty))
       (gnus-group-mail-4-empty-face  (:inherit gnus-group-news-4-empty))
       (gnus-group-mail-5-face  (:weight bold :inherit gnus-group-mail-5-empty))
       (gnus-group-mail-5-empty-face  (:inherit gnus-group-news-5-empty))
       (gnus-group-mail-6-face  (:weight bold :inherit gnus-group-mail-6-empty))
       (gnus-group-mail-6-empty-face  (:inherit gnus-group-news-6-empty))
       (gnus-group-mail-low-face  (:weight bold :inherit gnus-group-mail-low-empty))
       (gnus-group-mail-low-empty-face  (:inherit gnus-group-news-low-empty))
       (gnus-group-news-1-face  (:weight bold :inherit gnus-group-news-1-empty))
       (gnus-group-news-2-face  (:weight bold :inherit gnus-group-news-2-empty))
       (gnus-group-news-3-face  (:weight bold :inherit gnus-group-news-3-empty))
       (gnus-group-news-4-face  (:weight bold :inherit gnus-group-news-4-empty))
       (gnus-group-news-5-face  (:weight bold :inherit gnus-group-news-5-empty))
       (gnus-group-news-6-face  (:weight bold :inherit gnus-group-news-6-empty))
       (gnus-group-news-low-face  (:weight bold :inherit gnus-group-news-low-empty))
       (gnus-header-content-face  (:inherit message-header-other))
       (gnus-header-from-face  (:inherit message-header-from))
       (gnus-header-name-face  (:inherit message-header-name))
       (gnus-header-newsgroups-face  (:inherit message-header-other))
       (gnus-header-subject-face  (:inherit message-header-subject))
       (gnus-summary-cancelled-face  (:foreground orange))
       (gnus-summary-high-ancient-face  (:foreground blue))
       (gnus-summary-high-read-face  (:foreground green :weight bold))
       (gnus-summary-high-ticked-face  (:foreground orange :weight bold))
       (gnus-summary-high-unread-face  (:foreground base00 :weight bold))
       (gnus-summary-low-ancient-face  (:foreground blue))
       (gnus-summary-low-read-face  (:foreground green))
       (gnus-summary-low-ticked-face  (:foreground orange :weight bold))
       (gnus-summary-low-unread-face  (:foreground base00))
       (gnus-summary-normal-ancient-face  (:foreground blue))
       (gnus-summary-normal-read-face  (:foreground green))
       (gnus-summary-normal-ticked-face  (:foreground orange :weight bold))
       (gnus-summary-normal-unread-face  (:foreground base00))
       (gnus-summary-selected-face  (:foreground yellow :weight bold))
       (gnus-cite-1-face  (:foreground blue))
       (gnus-cite-10-face  (:foreground yellow))
       (gnus-cite-11-face  (:foreground yellow))
       (gnus-cite-2-face  (:foreground blue))
       (gnus-cite-3-face  (:foreground blue))
       (gnus-cite-4-face  (:foreground green))
       (gnus-cite-5-face  (:foreground green))
       (gnus-cite-6-face  (:foreground green))
       (gnus-cite-7-face  (:foreground red))
       (gnus-cite-8-face  (:foreground red))
       (gnus-cite-9-face  (:foreground red))
       (gnus-group-news-1-empty-face  (:foreground yellow))
       (gnus-group-news-2-empty-face  (:foreground green))
       (gnus-group-news-3-empty-face  (:foreground green))
       (gnus-group-news-4-empty-face  (:foreground blue))
       (gnus-group-news-5-empty-face  (:foreground blue))
       (gnus-group-news-6-empty-face  (:foreground base3))
       (gnus-group-news-low-empty-face  (:foreground base3))
       (gnus-signature-face  (:foreground yellow))
       (gnus-x-face  (:background base00 :foreground base3))

       ;; helm (these probably needs tweaking)
       (helm-apt-deinstalled  (:foreground base1))
       (helm-apt-installed  (:foreground green))
       (helm-bookmark-directory  (:inherit helm-ff-directory))
       (helm-bookmark-file  (:foreground base00))
       (helm-bookmark-gnus  (:foreground cyan))
       (helm-bookmark-info  (:foreground green))
       (helm-bookmark-man  (:foreground violet))
       (helm-bookmark-w3m  (:foreground yellow))
       (helm-bookmarks-su  (:foreground orange))
       (helm-buffer-not-saved  (:foreground orange))
       (helm-buffer-saved-out  (:foreground red :background base3
                                            :inverse-video t))
       (helm-buffer-size  (:foreground base1))
       (helm-candidate-number  (:background base2 :foreground base01
                                            :bold t))
       (helm-ff-directory  (:background base3  :foreground blue))
       (helm-ff-executable  (:foreground green))
       (helm-ff-file  (:background base3 :foreground base00))
       (helm-ff-invalid-symlink  (:background base3 :foreground orange
                                              :slant italic))
       (helm-ff-prefix  (:background yellow :foreground base3))
       (helm-ff-symlink  (:foreground cyan))
       (helm-grep-file  (:foreground cyan :underline t))
       (helm-grep-finish  (:foreground green))
       (helm-grep-lineno  (:foreground orange))
       (helm-grep-match  (:inherit match))
       (helm-grep-running  (:foreground red))
       (helm-header  (:inherit header-line))
       (helm-lisp-completion-info  (:foreground base00))
       (helm-lisp-show-completion  (:foreground yellow  :background base2
                                                :bold t))
       (helm-M-x-key  (:foreground orange :underline t))
       (helm-match  (:inherit match))
       (helm-selection  (:background base2 :underline t))
       (helm-selection-line  (:background base2 :foreground base01
                                          :underline nil))
       (helm-separator  (:foreground red))
       (helm-source-header  (:background blue-lc :foreground base3
                                         :underline nil))
       (helm-time-zone-current  (:foreground green))
       (helm-time-zone-home  (:foreground red))
       (helm-visible-mark  (:background base3 :foreground magenta :bold t))

       ;; hi-lock-mode
       (hi-yellow  (:foreground yellow-lc :background yellow-hc))
       (hi-pink  (:foreground magenta-lc :background magenta-hc))
       (hi-green  (:foreground green-lc :background green-hc))
       (hi-blue  (:foreground blue-lc :background blue-hc))
       (hi-black-b  (:foreground base01 :background base3 :weight bold))
       (hi-blue-b  (:foreground blue-lc :weight bold))
       (hi-green-b  (:foreground green-lc :weight bold))
       (hi-red-b  (:foreground red :weight bold))
       (hi-black-hb  (:foreground base01 :background base3 :weight bold))

       ;; highlight-changes
       (highlight-changes  (:foreground orange))
       (highlight-changes-delete  (:foreground red :underline t))

       ;; hl-line-mode
       (hl-line  (:background base2))
       (hl-line-face  (:background base2))

       ;; ido-mode
       (ido-first-match  (:foreground green :weight bold))
       (ido-only-match  (:foreground base3 :background green :weight bold))
       (ido-subdir  (:foreground blue))
       (ido-incomplete-regexp  (:foreground red :weight bold ))
       (ido-indicator  (:background red :foreground base3 :width condensed))
       (ido-virtual  (:foreground cyan))

       ;; linum-mode
       (linum  (:foreground base00 :background base3))

       ;; magit
       (magit-section-title  (:foreground yellow :weight bold))
       (magit-branch  (:foreground orange :weight bold))
       (magit-item-highlight  (:background base2))
       (magit-log-graph  (:foreground base1))
       (magit-log-head-label-bisect-bad  (:background red-hc :foreground red-lc :box (:line-width 1)))
       (magit-log-head-label-bisect-good  (:background green-hc :foreground green-lc
                                                       :box (:line-width 1)))
       (magit-log-head-label-default  (:background base2 :box (:line-width 1)))
       (magit-log-head-label-local  (:background blue-lc :foreground blue-hc :box (:line-width 1)))
       (magit-log-head-label-patches  (:background red-lc :foreground red-hc :box (:line-width 1)))
       (magit-log-head-label-remote  (:background green-lc :foreground green-hc :box (:line-width 1)))
       (magit-log-head-label-tags  (:background yellow-lc :foreground yellow-hc :box (:line-width 1)))
       (magit-log-sha1  (:foreground yellow))

       ;; message-mode
       (message-cited-text  (:foreground base1))
       (message-header-name  (:foreground green))
       (message-header-other  (:foreground green))
       (message-header-to  (:foreground yellow :weight bold))
       (message-header-cc  (:foreground orange :weight bold))
       (message-header-newsgroups  (:foreground yellow :weight bold))
       (message-header-subject  (:foreground orange))
       (message-header-xheader  (:foreground cyan))
       (message-mml  (:foreground yellow :weight bold))
       (message-separator  (:foreground base1 :slant italic))

       ;; mew
       (mew-face-header-subject  (:foreground orange))
       (mew-face-header-from  (:foreground yellow))
       (mew-face-header-date  (:foreground green))
       (mew-face-header-to  (:foreground red))
       (mew-face-header-key  (:foreground green))
       (mew-face-header-private  (:foreground green))
       (mew-face-header-important  (:foreground blue))
       (mew-face-header-marginal  (:foreground base00 :weight bold))
       (mew-face-header-warning  (:foreground red))
       (mew-face-header-xmew  (:foreground green))
       (mew-face-header-xmew-bad  (:foreground red))
       (mew-face-body-url  (:foreground orange))
       (mew-face-body-comment  (:foreground base00 :slant italic))
       (mew-face-body-cite1  (:foreground green))
       (mew-face-body-cite2  (:foreground blue))
       (mew-face-body-cite3  (:foreground orange))
       (mew-face-body-cite4  (:foreground yellow))
       (mew-face-body-cite5  (:foreground red))
       (mew-face-mark-review  (:foreground blue))
       (mew-face-mark-escape  (:foreground green))
       (mew-face-mark-delete  (:foreground red))
       (mew-face-mark-unlink  (:foreground yellow))
       (mew-face-mark-refile  (:foreground green))
       (mew-face-mark-unread  (:foreground red))
       (mew-face-eof-message  (:foreground green))
       (mew-face-eof-part  (:foreground yellow))

       ;; mingus
       (mingus-directory-face  (:foreground blue))
       (mingus-pausing-face  (:foreground magenta))
       (mingus-playing-face  (:foreground cyan))
       (mingus-playlist-face  (:foreground cyan ))
       (mingus-song-file-face  (:foreground yellow))
       (mingus-stopped-face  (:foreground red))

       ;; moccur
       (moccur-current-line-face  (:underline t))
       (moccur-edit-done-face
        (:foreground base1
                     :background base3
                     :slant italic))
       (moccur-edit-face
        (:background yellow :foreground base3))
       (moccur-edit-file-face  (:background base2))
       (moccur-edit-reject-face  (:foreground red))
       (moccur-face  (:background base2 :foreground base01
                                  :weight bold))

       ;; mu4e
       (mu4e-cited-1-face  (:foreground green :slant italic :weight normal))
       (mu4e-cited-2-face  (:foreground blue :slant italic :weight normal))
       (mu4e-cited-3-face  (:foreground orange :slant italic :weight normal))
       (mu4e-cited-4-face  (:foreground yellow :slant italic :weight normal))
       (mu4e-cited-5-face  (:foreground cyan :slant italic :weight normal))
       (mu4e-cited-6-face  (:foreground green :slant italic :weight normal))
       (mu4e-cited-7-face  (:foreground blue :slant italic :weight normal))
       (mu4e-flagged-face  (:foreground magenta :weight bold))
       (mu4e-view-url-number-face  (:foreground orange :weight bold))
       (mu4e-warning-face  (:foreground red :slant normal :weight bold))

       ;; mumamo
       (mumamo-background-chunk-submode1  (:background base2))

       ;; nav
       (nav-face-heading  (:foreground yellow))
       (nav-face-button-num  (:foreground cyan))
       (nav-face-dir  (:foreground green))
       (nav-face-hdir  (:foreground red))
       (nav-face-file  (:foreground base00))
       (nav-face-hfile  (:foreground red))

       ;; nav-flash
       (nav-flash-face  (:foreground orange :background base2))

       ;; org-mode
       (org-agenda-structure
        (:inherit font-lock-comment-face :foreground magenta :inverse-video t))
       (org-agenda-date
        (:foreground base00 :background base2 :weight bold
                     :box (:line-width 4 :color base2)))
       (org-agenda-date-weekend  (:inherit org-agenda-date :slant italic))
       (org-agenda-date-today
        (:inherit org-agenda-date :slant italic underline: t))
       (org-agenda-done  (:foreground green))
       (org-archived  (:foreground base1 :weight normal))
       (org-block  (:foreground base1))
       (org-block-begin-line  (:foreground base1 :slant italic))
       (org-checkbox  (:background base3 :foreground base00
                                   :box (:line-width 1 :style released-button)))
       (org-code  (:foreground base1))
       (org-date  (:foreground blue :underline t))
       (org-done  (:weight bold :foreground green))
       (org-ellipsis  (:foreground base1))
       (org-formula  (:foreground yellow))
       (org-headline-done  (:foreground green))
       (org-hide  (:foreground base3))
       (org-level-1  (:foreground orange))
       (org-level-2  (:foreground green))
       (org-level-3  (:foreground blue))
       (org-level-4  (:foreground yellow))
       (org-level-5  (:foreground cyan))
       (org-level-6  (:foreground green))
       (org-level-7  (:foreground red))
       (org-level-8  (:foreground blue))
       (org-link  (:foreground yellow :underline t))
       (org-sexp-date  (:foreground violet))
       (org-scheduled  (:foreground green))
       (org-scheduled-previously  (:foreground yellow))
       (org-scheduled-today  (:foreground blue :weight bold))
       (org-special-keyword  (:foreground base1 :weight bold))
       (org-table  (:foreground green))
       (org-tag  (:weight bold))
       (org-time-grid  (:foreground cyan))
       (org-todo  (:foreground red :weight bold))
       (org-upcoming-deadline  (:foreground yellow))
       (org-warning  (:foreground orange :weight bold :underline t))
       ;; org-habit (clear=blue ready=green alert=yellow overdue=red. future=lower contrast)
       (org-habit-clear-face  (:background blue-lc :foreground blue-hc))
       (org-habit-clear-future-face  (:background blue-lc))
       (org-habit-ready-face  (:background green-lc :foreground green))
       (org-habit-ready-future-face  (:background green-lc))
       (org-habit-alert-face  (:background yellow :foreground yellow-lc))
       (org-habit-alert-future-face  (:background yellow-lc))
       (org-habit-overdue-face  (:background red :foreground red-lc))
       (org-habit-overdue-future-face  (:background red-lc))
       ;; latest additions
       (org-agenda-dimmed-todo-face  (:foreground base1))
       (org-agenda-restriction-lock  (:background yellow))
       (org-clock-overlay  (:background yellow))
       (org-column  (:background base2 :strike-through nil
                                 :underline nil :slant normal :weight normal))
       (org-column-title  (:background base2 :underline t :weight bold))
       (org-date-selected  (:foreground red :inverse-video t))
       (org-document-info  (:foreground base00))
       (org-document-title  (:foreground base01  :weight bold :height 1.44))
       (org-drawer  (:foreground cyan))
       (org-footnote  (:foreground magenta :underline t))
       (org-latex-and-export-specials  (:foreground orange))
       (org-mode-line-clock-overrun  (:inherit modeline :background red))

       ;; outline
       (outline-8  (:inherit default))
       (outline-7  (:inherit outline-8 :height 1.0))
       (outline-6  (:inherit outline-7 :height 1.0))
       (outline-5  (:inherit outline-6 :height 1.0))
       (outline-4  (:inherit outline-5 :height 1.0))
       (outline-3  (:inherit outline-4 :height 1.0))
       (outline-2  (:inherit outline-3 :height 1.0))
       (outline-1  (:inherit outline-2 :height 1.0))

       ;; pretty-mode
       (pretty-mode-symbol-face   (:foreground green))

       ;; rainbow-delimiters
       (rainbow-delimiters-depth-1-face  (:foreground cyan))
       (rainbow-delimiters-depth-2-face  (:foreground yellow))
       (rainbow-delimiters-depth-3-face  (:foreground blue))
       (rainbow-delimiters-depth-4-face  (:foreground orange))
       (rainbow-delimiters-depth-5-face  (:foreground green))
       (rainbow-delimiters-depth-6-face  (:foreground yellow))
       (rainbow-delimiters-depth-7-face  (:foreground blue))
       (rainbow-delimiters-depth-8-face  (:foreground orange))
       (rainbow-delimiters-depth-9-face  (:foreground green))
       (rainbow-delimiters-depth-10-face  (:foreground yellow))
       (rainbow-delimiters-depth-11-face  (:foreground blue))
       (rainbow-delimiters-depth-12-face  (:foreground orange))
       (rainbow-delimiters-unmatched-face
        (:foreground base00 :background base3 :inverse-video t))

       ;; rst-mode
       (rst-level-1-face  (:background yellow   :foreground base3))
       (rst-level-2-face  (:background cyan    :foreground base3))
       (rst-level-3-face  (:background blue    :foreground base3))
       (rst-level-4-face  (:background violet  :foreground base3))
       (rst-level-5-face  (:background magenta :foreground base3))
       (rst-level-6-face  (:background red     :foreground base3))

       ;; rpm-mode
       (rpm-spec-dir-face  (:foreground green))
       (rpm-spec-doc-face  (:foreground green))
       (rpm-spec-ghost-face  (:foreground red))
       (rpm-spec-macro-face  (:foreground yellow))
       (rpm-spec-obsolete-tag-face  (:foreground red))
       (rpm-spec-package-face  (:foreground red))
       (rpm-spec-section-face  (:foreground yellow))
       (rpm-spec-tag-face  (:foreground blue))
       (rpm-spec-var-face  (:foreground red))

       ;; sh-mode
       (sh-quoted-exec  (:foreground violet :weight bold))
       (sh-escaped-newline  (:foreground yellow :weight bold))
       (sh-heredoc  (:foreground yellow :weight bold))

       ;; show-paren
       (show-paren-match
        (:foreground cyan :background base3 :weight normal :inverse-video t))
       (show-paren-mismatch
        (:foreground red :background base3 :weight normal :inverse-video t))

       ;; mic-paren
       (paren-face-match
        (:foreground cyan :background base3 :weight normal :inverse-video t))
       (paren-face-mismatch
        (:foreground red :background base3 :weight normal :inverse-video t))
       (paren-face-no-match
        (:foreground red :background base3 :weight normal :inverse-video t))

       ;; SLIME
       (slime-repl-inputed-output-face  (:foreground red))

       ;; speedbar
       (speedbar-button-face  (:inherit variable-pitch :foreground base1))
       (speedbar-directory-face  (:inherit variable-pitch :foreground blue))
       (speedbar-file-face  (:inherit variable-pitch :foreground base00))
       (speedbar-highlight-face  (:inherit variable-pitch :background base2))
       (speedbar-selected-face  (:inherit variable-pitch :foreground yellow :underline t))
       (speedbar-separator-face  (:inherit variable-pitch
                                           :background blue :foreground base3
                                           :overline (:color ecyan-lc)))
       (speedbar-tag-face  (:inherit variable-pitch :foreground green))

       ;; sunrise commander headings
       (sr-active-path-face  (:background blue :foreground base3
                                          :height 100  :weight bold))
       (sr-editing-path-face  (:background yellow :foreground base3
                                           :weight bold :height 100))
       (sr-highlight-path-face  (:background green :foreground base3
                                             :weight bold :height 100))
       (sr-passive-path-face  (:background base1 :foreground base3
                                           :weight bold :height 100))
       ;; sunrise commander marked
       (sr-marked-dir-face  (:inherit dired-marked))
       (sr-marked-file-face  (:inherit dired-marked))
       (sr-alt-marked-dir-face  (:background magenta :foreground base3
                                             :weight bold))
       (sr-alt-marked-file-face  (:background magenta :foreground base3
                                              :weight bold))
       ;; sunrise commander fstat
       (sr-directory-face  (:inherit dired-directory :weight normal))
       (sr-symlink-directory-face  (:inherit dired-directory :slant italic :weight normal))
       (sr-symlink-face  (:inherit dired-symlink :slant italic :weight normal))
       (sr-broken-link-face  (:inherit dired-warning :slant italic :weight normal))
       ;; sunrise commander file types
       (sr-compressed-face  (:foreground base00))
       (sr-encrypted-face  (:foreground base00))
       (sr-log-face  (:foreground base00))
       (sr-packaged-face  (:foreground base00))
       (sr-html-face  (:foreground base00))
       (sr-xml-face  (:foreground base00))
       ;; sunrise commander misc
       (sr-clex-hotchar-face  (:background red  :foreground base3 :weight bold))

       ;; table
       (table-cell  (:foreground base00 :background base2))

       ;; tooltip. (NOTE: This setting has no effect on the os widgets for me
       ;; zencoding uses this)
       (tooltip  (:background yellow-lc :foreground yellow-hc
                              :inherit variable-pitch))

       ;; tuareg
       (tuareg-font-lock-governing-face  (:foreground magenta :weight bold))
       (tuareg-font-lock-multistage-face  (:foreground blue :background base2 :weight bold))
       (tuareg-font-lock-operator-face  (:foreground base01))
       (tuareg-font-lock-error-face  (:foreground yellow :background red :weight bold))
       (tuareg-font-lock-interactive-output-face  (:foreground cyan))
       (tuareg-font-lock-interactive-error-face  (:foreground red))

       ;; undo-tree
       (undo-tree-visualizer-default-face
        (:foreground base1 :background base3))
       (undo-tree-visualizer-current-face  (:foreground cyan :inverse-video t))
       (undo-tree-visualizer-active-branch-face
        (:foreground base01 :background base3 :weight bold))
       (undo-tree-visualizer-register-face  (:foreground yellow))

       ;; volatile highlights
       (vhl/default-face  (:background green-lc :foreground green-hc))

       ;; w3m
       (w3m-anchor  (:inherit link))
       (w3m-arrived-anchor  (:inherit link-visited))
       (w3m-form  (:background base03 :foreground base00))
       (w3m-header-line-location-title  (:background base02 :foreground yellow))
       (w3m-header-line-location-content  (:background base02 :foreground base00))
       (w3m-bold  (:foreground base01 :weight bold))
       (w3m-image-anchor  (:background base3 :foreground cyan :inherit link))
       (w3m-image  (:background base3 :foreground cyan))
       (w3m-lnum-minibuffer-prompt  (:foreground base01))
       (w3m-lnum-match  (:background base2))
       (w3m-lnum  (:underline nil :bold nil :foreground red))

       ;; whitespace-mode
       (whitespace-space  (:background base3 :foreground yellow-lc
                                       :inverse-video t))
       (whitespace-hspace  (:background base3 :foreground red-lc
                                        :inverse-video t))
       (whitespace-tab  (:background base3 :foreground orange-lc
                                     :inverse-video t))
       (whitespace-newline  (:foreground base1))
       (whitespace-trailing  (:foreground blue-lc :background base3
                                          :inverse-video t))
                                        ; removing inverse video on this
       (whitespace-line  (:background base3 :foreground magenta
                                      :inverse-video nil))
       (whitespace-space-before-tab  (:background base3 :foreground green-lc
                                                  :inverse-video t))
       (whitespace-indentation  (:background base3 :foreground magenta-lc
                                             :inverse-video t))
       (whitespace-empty  (:background base00 :foreground red-lc :inverse-video t))
       (whitespace-space-after-tab  (:background base3  :foreground violet-lc
                                                 :inverse-video t))

       ;; wanderlust
       (wl-highlight-folder-few-face  (:foreground red))
       (wl-highlight-folder-many-face  (:foreground red))
       (wl-highlight-folder-path-face  (:foreground orange))
       (wl-highlight-folder-unread-face  (:foreground blue))
       (wl-highlight-folder-zero-face  (:foreground base00))
       (wl-highlight-folder-unknown-face  (:foreground blue))
       (wl-highlight-message-citation-header  (:foreground red))
       (wl-highlight-message-cited-text-1  (:foreground red))
       (wl-highlight-message-cited-text-2  (:foreground green))
       (wl-highlight-message-cited-text-3  (:foreground blue))
       (wl-highlight-message-cited-text-4  (:foreground blue))
       (wl-highlight-message-header-contents-face  (:foreground green))
       (wl-highlight-message-headers-face  (:foreground red))
       (wl-highlight-message-important-header-contents  (:foreground green))
       (wl-highlight-message-header-contents  (:foreground green))
       (wl-highlight-message-important-header-contents2  (:foreground green))
       (wl-highlight-message-signature  (:foreground green))
       (wl-highlight-message-unimportant-header-contents  (:foreground base00))
       (wl-highlight-summary-answered-face  (:foreground blue))
       (wl-highlight-summary-disposed-face  (:foreground base00
                                                         :slant italic))
       (wl-highlight-summary-new-face  (:foreground blue))
       (wl-highlight-summary-normal-face  (:foreground base00))
       (wl-highlight-summary-thread-top-face  (:foreground yellow))
       (wl-highlight-thread-indent-face  (:foreground magenta))
       (wl-highlight-summary-refiled-face  (:foreground base00))
       (wl-highlight-summary-displaying-face  (:underline t :weight bold))

       ;; which-func-mode
       (which-func  (:foreground green))

       ;; window-number-mode
       (window-number-face  (:foreground green))

       ;; yascroll
       (yascroll:thumb-text-area
        (:foreground base1 :background base1))
       (yascroll:thumb-fringe
        (:foreground base1 :background base1))

       ;; zencoding
       (zencoding-preview-input  (:background base2 :box  (:color base01)))

       ) mode))


    ))

(defun create-solarized-theme (variant theme-name &optional childtheme)
  (let* ((defs (solarized-color-definitions variant))
         (theme-vars (mapcar (lambda (def) (list (car def) (cdr def)))
                             (cadr defs)))
         (theme-faces (car defs)))
    (apply 'custom-theme-set-faces theme-name theme-faces)
    (apply 'custom-theme-set-variables theme-name theme-vars))



    ;; call chained theme function
  ;;(when childtheme (funcall childtheme)))
)

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide 'solarized)

;;; solarized.el ends here.
