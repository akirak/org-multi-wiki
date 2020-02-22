;;; helm-org-multi-wiki.el --- Helm interface to org-multi-wiki -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org-multi-wiki "0.1") (org-ql "0.4") (helm-org-ql "0.4") (dash "2.12"))
;; Keywords: org, outlines
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

;; This package provides a helm interface to org-multi-wiki.

;;; Code:

(require 'dash)
(require 'org-multi-wiki)
(require 'org-ql)
(require 'helm-org-ql)

(defvar helm-org-multi-wiki-dummy-source-map
  (let ((map (copy-keymap helm-map)))
    map)
  "Keymap for the dummy source.
Based on `helm-map'.")

(defun helm-org-multi-wiki-create-entry-from-input (id)
  "Create an entry in ID from the input in the dummy source."
  (let ((inp (helm-get-selection)))
    (if (not (string-empty-p inp))
        (helm-run-after-exit #'org-multi-wiki-visit-entry inp :id id)
      (user-error "Input is empty"))))

;;;###autoload
(defmacro helm-org-multi-wiki-def-create-entry-action (id)
  "Define a command to creating an entry in ID via the dummy source."
  `(defun ,(intern (format "helm-org-multi-wiki-create/%s" id)) ()
     (interactive)
     (helm-org-multi-wiki-create-entry-from-input (quote ,id))))

(cl-defun helm-org-multi-wiki-select-ids (&key prompt action)
  "Select directory IDs using helm.

PROMPT and ACTION are passed to helm."
  (helm :prompt (or prompt "Wikis")
        :sources
        (list (helm-build-sync-source "Wiki ID"
                :candidates (mapcar (lambda (x)
                                      (cons (format "%s (%s)"
                                                    (car x)
                                                    (nth 1 x))
                                            (car x)))
                                    org-multi-wiki-directories)
                :action (or action (lambda (candidate) (or (helm-marked-candidates) candidate)))))))

(cl-defun helm-org-multi-wiki-make-dummy-source (ids &key first)
  "Create a dummy helm source.

IDS and FIRST are the same as in `helm-org-multi-wiki'."
  (helm-build-dummy-source "New entry"
    :keymap helm-org-multi-wiki-dummy-source-map
    :action
    (mapcar (lambda (id)
              (cons (format "Create a new entry in %s%s"
                            id
                            (if (equal id first)
                                " (current)"
                              ""))
                    (lambda (inp)
                      (org-multi-wiki-visit-entry inp :id id))))
            (if (and first (> (length ids) 1))
                (cons first (-remove-item first ids))
              ids))))

;;;###autoload
(cl-defun helm-org-multi-wiki (&optional ids &key first)
  "Visit an entry or create a new entry.

IDS are are a list of directory ids.
It can be a list of symbols or a symbol.

When FIRST is given, it is the default target of entry creation."
  (interactive)
  ;; Based on the implementation of helm-org-ql.
  (pcase current-prefix-arg
    ('(4) (let ((current-prefix-arg nil))
            (helm-org-multi-wiki-select-ids
             :action
             (list (cons "Run helm-org-mult-wiki on selected IDs"
                         (lambda (_)
                           (helm-org-multi-wiki (helm-marked-candidates))))))))
    (_ (let* ((ids (cl-etypecase ids
                     ;; Normalize IDs to make it a list of symbols.
                     (null (list org-multi-wiki-current-directory-id))
                     (list ids)
                     (symbol (list ids))))
              (boolean 'and)
              (helm-input-idle-delay helm-org-ql-input-idle-delay)
              (files (->> ids
                          (mapcar #'org-multi-wiki-entry-files)
                          (apply #'append))))
         (helm :prompt (format "Query (boolean %s): " (-> boolean symbol-name upcase))
               :sources
               (list (helm-org-ql-source files
                                         :name (format "Wiki (%s)"
                                                       (mapconcat #'symbol-name ids ",")))
                     (helm-org-multi-wiki-make-dummy-source ids :first first)))))))

;;;###autoload
(defun helm-org-multi-wiki-all ()
  "Run `helm-org-multi-wiki' on all configured directories."
  (interactive)
  (helm-org-multi-wiki (mapcar #'car org-multi-wiki-directories)
                       :first org-multi-wiki-current-directory-id))

(provide 'helm-org-multi-wiki)
;;; helm-org-multi-wiki.el ends here
