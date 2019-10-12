;;; alpha-org.el --- A powerful Org configuration           -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: outlines

;; Package-Requires: ((emacs "26.3") (use-package) (general) (org-bullets) (org-make-toc) (org-sidebar) (org-sticky-header))

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

;;;; Footer

(provide 'alpha-org)

;;; alpha-org.el ends here
