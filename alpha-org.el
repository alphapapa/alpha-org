;;; alpha-org.el --- A powerful Org configuration           -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: outlines

;; Package-Requires: ((emacs "26.3") (use-package) (general) (avy) (which-key) (org-bullets) (org-make-toc) (org-sidebar) (org-sticky-header))

;;; License:

;; Copyright (C) 2019  Adam Porter

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `alpha-org' is a powerful configuration for `org-mode', similar to
;; how Spacemacs and Doom are configurations for Emacs as a whole.

;;; Code:

;;;; Requirements

(require 'use-package)

(use-package general
  :config
  (global-unset-key (kbd "M-SPC"))
  (general-create-definer alpha-org/general-def
    :prefix "M-SPC"))

(defun ap/org-avy-refile-as-child ()
  "Refile current heading as first child of heading selected with `avy.'"
  ;; Inspired by `org-teleport': http://kitchingroup.cheme.cmu.edu/blog/2016/03/18/Org-teleport-headlines/
  (interactive)
  ;; NOTE: Use `when-let' so that if avy is aborted with "C-g", `org-refile' won't be called with
  ;; a nil refile location.
  (when-let ((marker (ap/org-avy-marker)))
    (let* ((filename (buffer-file-name (or (buffer-base-buffer (marker-buffer marker))
                                           (marker-buffer marker))))
           (heading (org-with-point-at marker
                      (org-get-heading 'no-tags 'no-todo)))
           ;; NOTE: I guess this won't work with target buffers whose filename is nil, but I doubt
           ;; I'll ever want to do that.
           (rfloc (list heading filename nil marker))
           (org-after-refile-insert-hook (cons #'org-reveal org-after-refile-insert-hook)))
      (org-refile nil nil rfloc))))

(defun ap/org-avy-marker ()
  "Return marker at Org heading selected with avy."
  (save-excursion
    (when-let* ((org-reverse-note-order t)
                (pos (atypecase (avy-with avy-goto-line
                                  (avy--generic-jump (rx bol "*") nil avy-style))
                       ;; If avy is aborted with "C-g", it returns `t', so we know it was NOT
                       ;; aborted when it returns an int.  If it doesn't return an int, we return
                       ;; nil.
                       (integer it))))
      (copy-marker pos))))

;;;; Configuration

;;  This section includes configuration code for options and packages built-in to Org.

(use-package org-mode
  :custom
  (org-use-speed-commands (lambda ()
                            (and (looking-at org-outline-regexp)
                                 (looking-back "^\**")))))

(add-hook 'org-mode-hook 'org-indent-mode)

;;;; Packages

(use-package org-sidebar
  :general
  (alpha-org/general-def
   "vs" #'org-sidebar-toggle
   "vt" #'org-sidebar-tree-toggle)
  :custom (org-sidebar-tree-side 'left))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-sticky-header
  :hook (org-mode . org-sticky-header-mode))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(defun ap/org-refile-within-buffer ()
  "Call `org-refile' with `org-refile-targets' set to current buffer's headings."
  ;; This works now, but it doesn't fontify the headings/paths like
  ;; Helm does, so it's faster but doesn't look as nice
  (interactive)
  (let ((org-refile-use-cache nil)
        (org-refile-use-outline-path t)
        (org-refile-targets (list (cons (list (buffer-file-name (or (buffer-base-buffer (current-buffer))
                                                                    (current-buffer))))
                                        (cons :maxlevel 20)))))
    (call-interactively 'org-refile)))

;;;; Footer

(provide 'alpha-org)

;;; alpha-org.el ends here
