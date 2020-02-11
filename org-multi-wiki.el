;;; org-multi-wiki.el --- Multiple wikis based on Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12") (s "1.12"))
;; Keywords: org outlines files
;; URL: https://github.com/akirak/org-multi-wiki

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides an infrastructure for multiple wikis on a
;; single machine, based on org-mode.

;; For configuration, see README.

;; For practical use, see helm-org-multi-wiki.el.

;;; Code:

(require 'subr-x)
(require 'dash)
(require 's)
(require 'org)

(defgroup org-multi-wiki nil
  "Multiple wikis based on org-mode."
  :group 'org)

;;;;  Custom variables
(defcustom org-multi-wiki-directories
  `((default ,org-directory))
  "List of directories containing wiki entries.

Each entry in this variable should be a list containing the
following items, in that order:

- A symbol to uniquely identify the directory.
- A file path to the directory containing Org files."
  :type '(repeat (list (symbol :tag "ID")
                       (directory :tag "Directory")))
  :group 'org-multi-wiki)

(defcustom org-multi-wiki-default-directory-id (caar org-multi-wiki-directories)
  "ID of the default wiki directory.

This should be the first element of one of the entries in
`org-multi-wiki-directories'."
  :type 'symbol
  :group 'org-multi-wiki)

(defcustom org-multi-wiki-escape-file-name-fn #'org-multi-wiki-escape-file-name-camelcase-1
  "Function used to generated an escaped file name from a heading."
  :type 'function
  :group 'org-multi-wiki)

(defcustom org-multi-wiki-entry-template-fn #'org-multi-wiki-default-entry-template-fn
  "Function to create an initial Org entry from a heading."
  :type 'function
  :group 'org-multi-wiki)

(defcustom org-multi-wiki-find-file-fn #'find-file-other-window
  "Function used to visit an Org file."
  :type 'function
  :group 'org-multi-wiki)

(defcustom org-multi-wiki-ignore-missing-directories nil
  "When non-nil, return an empty result from `org-multi-wiki-entry-files' when the directory does not exist."
  :type 'boolean
  :group 'org-multi-wiki)

;;;; Other variables
(defvar org-multi-wiki-current-directory-id org-multi-wiki-default-directory-id)

;;;; Default functions
(defun org-multi-wiki-escape-file-name-camelcase-1 (heading)
  "Escape HEADING suitable for use in file name."
  (cl-labels ((filename-escape
               (str)
               (s-replace-regexp (rx (not (any alnum "-._" nonascii))) "" str)))
    (let ((words (split-string heading (rx (any space)))))
      (if (= 1 (length words))
          (filename-escape (car words))
        (->> words
             (-map #'filename-escape)
             (-map #'s-upper-camel-case)
             (string-join))))))

(defun org-multi-wiki-default-entry-template-fn (heading)
  "Generate an Org entry from HEADING."
  (concat "* " heading "\n"))

;;;; File and directory infrastructure
(defun org-multi-wiki-directory (&optional id)
  "Get the directory of a wiki ID."
  (let ((id (or id org-multi-wiki-current-directory-id)))
    (or (car-safe (alist-get id org-multi-wiki-directories))
        (error "No entry exists for %s in org-multi-wiki-directories" id))))

(defun org-multi-wiki-select-directory-id (&optional prompt)
  "Select a wiki id using `completing-read', with an optional PROMPT."
  (intern (completing-read (or prompt "Wiki: ")
                           (mapcar #'car org-multi-wiki-directories))))

;;;###autoload
(defun org-multi-wiki-entry-files (&optional id)
  "Get a list of Org files in the directory with ID."
  (directory-files (org-multi-wiki-directory id) t org-agenda-file-regexp))

;;;; Commands

;;;###autoload
(defun org-multi-wiki-switch (id)
  "Set the current wiki to ID."
  (interactive (list (org-multi-wiki-select-directory-id)))
  (when-let (dir (org-multi-wiki-directory id))
    (setq org-multi-wiki-current-directory-id id)
    (message "Set the current wiki to \"%s\" (%s)" id
             org-multi-wiki-current-directory-id)))

;;;###autoload
(cl-defun org-multi-wiki-visit-entry (heading &key id)
  "Visit an Org file for HEADING in the directory with ID."
  (let* ((dir (org-multi-wiki-directory id))
         (filename (concat (funcall org-multi-wiki-escape-file-name-fn heading) ".org"))
         (fpath (expand-file-name filename dir)))
    (unless (and dir (file-directory-p dir))
      (user-error "Wiki directory is nil or missing: %s" dir))
    (unless (file-exists-p fpath)
      ;; Set default-directory to allow directory-specific templates
      (let ((default-directory dir))
        (with-temp-buffer
          (insert (funcall org-multi-wiki-entry-template-fn heading))
          (write-file fpath))))
    (funcall org-multi-wiki-find-file-fn fpath)))

(provide 'org-multi-wiki)
;;; org-multi-wiki.el ends here
