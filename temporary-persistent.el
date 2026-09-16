;;; temporary-persistent.el --- Keep temp notes buffers persistent -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2026 Kostafey <kostafey@gmail.com>

;; Author: Kostafey <kostafey@gmail.com>
;; URL: https://github.com/kostafey/temporary-persistent
;; Keywords: temp, buffers, notes
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (names "20151201.0") (dash "2.12.1") (s "1.10.0"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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

;; temporary-persistent.el -  easy way to switch temp buffers and keep them
;; persistent. It provides `temporary-persistent-switch-buffer' function
;; to create temporary buffers named *temp*, *temp-1* and so on, witch is
;; associated to files and will be saved any time you run `kill-buffer' or
;; `kill-emacs'.
;; Furtermore, you can save them manually any time via `save-buffer' function.
;; If `consult' is installed, `temporary-persistent-consult-switch-buffer'
;; lists the temp buffers annotated with their contents summary.
;; See README.md for more information.

;;; Code:

(require 's)
(require 'dash)
(require 'names)

(declare-function consult-buffer "consult" (&optional sources))
(declare-function consult--buffer-query "consult")
(declare-function consult--buffer-state "consult")

(defgroup temporary-persistent nil
  "Keep temp notes buffers persistent."
  :group 'convenience)

(define-namespace temporary-persistent-

(defcustom default-major-mode 'fundamental-mode
  "New temp buffer `major-mode'."
  :type 'symbol
  :group 'temporary-persistent)

(defcustom default-submodes (list 'auto-fill-mode)
  "List of submodes enabled in new temp buffer."
  :type 'list
  :group 'temporary-persistent)

(defcustom store-folder "~/temp"
  "Directory to keep files with temporary buffers content."
  :type 'string
  :group 'temporary-persistent)

(defcustom buffer-name-template "temp"
  "Template for temporary buffers names."
  :type 'string
  :group 'temporary-persistent)

(defun buffer-name-regexp ()
  "Return regexp matching temp buffers names.
See `buffer-name-template'."
  (concat "\\`\\*" (regexp-quote buffer-name-template) "\\(-[0-9]+\\)?\\*\\'"))

(defun buffer-p (buffer)
  "Return non-nil when BUFFER is a temp buffer."
  (and (buffer-live-p buffer)
       (string-match-p (buffer-name-regexp) (buffer-name buffer))))

(defun save-and-kill-buffer ()
  "Save buffer contents and kill buffer."
  (save-buffer)
  (set (make-local-variable 'kill-buffer-query-functions) nil)
  (kill-buffer (current-buffer)))

(defun save-all-related-buffers ()
  "Save all buffers corresponding to `buffer-name-template'."
  (-map
   (lambda (buf)
     (if (buffer-p buf)
         (save-buffer buf)))
   (buffer-list)))

:autoload
(defun switch-buffer (&optional num)
  "Switch to temp buffer."
  (interactive "P")
  (let* ((temp-file-name (if (and num (numberp num))
                             (concat buffer-name-template
                                     "-"
                                     (int-to-string num))
                           buffer-name-template))
         (temp-file-path (progn
                           (unless (file-exists-p store-folder)
                             (make-directory store-folder t))
                           (expand-file-name temp-file-name store-folder)))
         (temp-buffer-name (concat "*" temp-file-name "*")
                           buffer-name-template))
    (if (not (get-buffer temp-buffer-name))
        (progn
          (find-file temp-file-path)
          (rename-buffer temp-buffer-name)
          (when (fboundp default-major-mode)
            (funcall-interactively default-major-mode))
          (-map (lambda (mode)
                  (when (fboundp mode)
                    (funcall mode t)))
                default-submodes))
      (switch-to-buffer temp-buffer-name))
    (set (make-local-variable 'kill-buffer-query-functions)
         'temporary-persistent-save-and-kill-buffer)))


;;; Buffer contents summary

(defconst -markdown-heading-regexp "^#[ \t]+\\(.*?\\)[ \t]*#*[ \t]*$"
  "Regexp matching the level 1 `markdown-mode' heading.")

(defconst -org-title-regexp "^[ \t]*#\\+title:[ \t]*\\(.*?\\)[ \t]*$"
  "Regexp matching the `org-mode' title keyword.")

(defconst -org-heading-regexp "^\\*[ \t]+\\(.*?\\)[ \t]*$"
  "Regexp matching the level 1 `org-mode' heading.")

(defconst -non-blank-line-regexp "^[ \t]*\\([^ \t\n].*?\\)[ \t]*$"
  "Regexp matching any non-blank line.")

(defun -match-line (regexp)
  "Return the first REGEXP match (group 1) in the current buffer.
Return nil when there is no match or the matched text is blank."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t))
        (when (re-search-forward regexp nil t)
          (let ((line (s-trim (match-string-no-properties 1))))
            (unless (s-blank? line) line)))))))

(defun buffer-summary (buffer)
  "Return one line describing the contents of BUFFER.
It is the first level 1 heading for `markdown-mode' buffers, the
`#+title:' keyword or, lacking it, the first level 1 heading for
`org-mode' buffers.  In any other mode, or when no such heading is
found, the first non-blank line of the buffer is returned."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer
        (or (cond
             ((derived-mode-p 'markdown-mode)
              (-match-line -markdown-heading-regexp))
             ((derived-mode-p 'org-mode)
              (or (-match-line -org-title-regexp)
                  (-match-line -org-heading-regexp))))
            (-match-line -non-blank-line-regexp)
            ""))
    ""))


;;; `consult-buffer' integration

(defcustom consult-mode-width 20
  "Width of the `major-mode' column of `consult-switch-buffer'."
  :type 'integer
  :group 'temporary-persistent)

(defcustom consult-summary-width 80
  "Maximum width of the summary column of `consult-switch-buffer'."
  :type 'integer
  :group 'temporary-persistent)

(defcustom consult-mode-face 'completions-annotations
  "Face of the `major-mode' column of `consult-switch-buffer'."
  :type 'face
  :group 'temporary-persistent)

(defcustom consult-summary-face 'completions-annotations
  "Face of the summary column of `consult-switch-buffer'."
  :type 'face
  :group 'temporary-persistent)

(defun -consult-items ()
  "Return the list of temp buffers as `consult-buffer' candidates."
  (consult--buffer-query
   :sort 'visibility
   :exclude nil
   :include (list (buffer-name-regexp))
   :as (lambda (buffer) (cons (buffer-name buffer) buffer))))

(defun -consult-annotate (buffer)
  "Annotate BUFFER with its `major-mode' and its contents summary.
BUFFER is a buffer or a buffer name."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))))
    (when (buffer-live-p buffer)
      (concat
       (propertize (truncate-string-to-width
                    (format-mode-line 'mode-name nil nil buffer)
                    consult-mode-width 0 ?\s t)
                   'face consult-mode-face)
       " "
       (propertize (truncate-string-to-width
                    (buffer-summary buffer)
                    consult-summary-width 0 nil t)
                   'face consult-summary-face)))))

(defvar consult-source
  (list :name     "Temp Buffer"
        :narrow   ?t
        :category 'temporary-persistent-buffer
        :face     'consult-buffer
        :history  'buffer-name-history
        :state    #'consult--buffer-state
        :annotate #'-consult-annotate
        :items    #'-consult-items
        :default  t)
  "Temp buffers source for `consult-buffer'.
Unlike `consult-source-buffer', it annotates the candidates with a
summary of their contents instead of their file path, see
`buffer-summary'.")

:autoload
(defun consult-switch-buffer ()
  "Switch to a temp buffer, selecting it with `consult-buffer'.
Every candidate is annotated with its `major-mode' and a summary of
its contents, see `buffer-summary'."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "The `consult' package is not available"))
  (unless (-consult-items)
    (user-error "No temp buffer to switch to"))
  (consult-buffer (list consult-source)))
)

(add-hook 'kill-emacs-hook 'temporary-persistent-save-all-related-buffers)

(provide 'temporary-persistent)

;;; temporary-persistent.el ends here
