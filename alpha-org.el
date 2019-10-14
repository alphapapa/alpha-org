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

;;;; Configuration

;;  This section includes configuration code for options and packages built-in to Org.

(use-package org
  :custom (org-ellipsis "⋯"))

(use-package org
  :custom
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+"))))

(use-package org
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

(use-package org-web-tools
  :general
  (alpha-org/general-def
    "ilu" #'org-web-tools-link-for-url))

(use-package org-recent-headings
  :general
  (alpha-org/general-def
    "hr" #'org-recent-headings-helm)
  :config
  (org-recent-headings-mode)
  :custom
  (org-recent-headings-reverse-paths t)
  (org-recent-headings-candidate-number-limit 100))

(use-package helm-org
  :general
  (alpha-org/general-def
    "ha" #'helm-org-agenda-files-headings
    "hb" #'helm-org-in-buffer-headings
    "hp" #'helm-org-parent-headings)
  :custom
  (helm-org-format-outline-path t))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package avy
  :general
  (alpha-org/general-def
    "hv" #'alpha-org/goto-visible-heading
    "rv" #'alpha-org/refile-to-visible)

  :config
  (defun alpha-org/refile-to-visible ()
    "Refile current heading as first child of visible heading selected with Avy."
    ;; Inspired by `org-teleport':
    ;; http://kitchingroup.cheme.cmu.edu/blog/2016/03/18/Org-teleport-headlines/
    (interactive)
    ;; NOTE: Use `when-let' so that if avy is aborted with "C-g",
    ;; `org-refile' won't be called with a nil refile location.
    (when-let ((marker (alpha-org/avy-marker)))
      (let* ((filename (buffer-file-name (or (buffer-base-buffer
                                              (marker-buffer marker))
                                             (marker-buffer marker))))
             (heading (org-with-point-at marker
                        (org-get-heading 'no-tags 'no-todo)))
             ;; NOTE: I guess this won't work with target buffers
             ;; whose filename is nil, but I doubt I'll ever want to
             ;; do that.
             (rfloc (list heading filename nil marker))
             (org-after-refile-insert-hook (cons #'org-reveal org-after-refile-insert-hook)))
        (org-refile nil nil rfloc))))

  (defun alpha-org/goto-visible-heading ()
    "Go to visible heading selected with Avy."
    (interactive)
    (when-let* ((marker (alpha-org/avy-marker)))
      (with-current-buffer (marker-buffer marker)
        (goto-char marker))))

  (defun alpha-org/avy-marker ()
    "Return marker at Org heading selected with Avy."
    (save-excursion
      (when-let* ((org-reverse-note-order t)
                  (pos (avy-with avy-goto-line
                         (avy-jump (rx bol (1+ "*") (1+ blank))))))
        (when (integerp (car pos))
          ;; If avy is aborted with "C-g", it returns
          ;; `t', so we know it was NOT aborted when it
          ;; returns an int.  If it doesn't return an
          ;; int, we return nil.
          (copy-marker (car pos)))))))

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
