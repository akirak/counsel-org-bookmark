;;; counsel-org-bookmark.el --- Counsel interface to Org bookmarks -*- lexical-binding: t -*-

;; Copyright (C) 2018 by Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (org "9.0") (ivy "0.10"))
;; URL: https://github.com/akirak/counsel-org-bookmark

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

;; `counsel-org-bookmark' is a counsel interface that displays only bookmarks
;; to Org files.

;;; Code:

(defun counsel-org-bookmark ()
  "`counsel-bookmark' limited to Org bookmarks."
  (interactive)
  (bookmark-maybe-load-default-file)
  (ivy-read "Org bookmark:"
            'counsel-org-bookmark--entries
            :caller 'counsel-org-bookmark
            :action (lambda (name)
                      (when-let ((record (assoc name bookmark-alist)))
                        (with-ivy-window (bookmark-jump record))))))

(defun counsel-org-bookmark--entries (_str _collection _predicate)
  "Return a list of Org bookmark candidates."
  (thread-last bookmark-alist
    (cl-remove-if-not (lambda (record)
                        (string-suffix-p ".org" (bookmark-get-filename record))))
    (mapcar #'car)))

(ivy-add-actions
 'counsel-org-bookmark
 '(("j" (lambda (name)
          (when-let ((record (assoc name bookmark-alist)))
            (with-ivy-window (bookmark-jump-other-window record))))
    "other window")
   ("e" bookmark-rename "edit")
   ("d" bookmark-delete "delete")
   ("l" (lambda (name)
          (when-let ((record (assoc name bookmark-alist)))
            (save-excursion
              (bookmark-handle-bookmark record)
              (call-interactively #'org-store-link))))
    "store link")
   ("c" (lambda (name)
          (when-let ((record (assoc name bookmark-alist)))
            (with-ivy-window
              (bookmark-jump record)
              (counsel-org-bookmark--create-child))))
    "create a child")
   ("r" (lambda (name)
          (when-let ((record (assoc name bookmark-alist)))
            (counsel-org-bookmark--go-deeper record)))
    "go deeper")))

(defun counsel-org-bookmark--create-child ()
  "Create a child under the current entry."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in org-mode"))
  (if-let ((level (org-current-level)))
      (progn
        (org-end-of-subtree t)
        (insert "\n" (make-string (org-get-valid-level (1+ level))
                                  ?\*) " "))
    (goto-char (point-max))
    (insert "\n* ")))

(defvar counsel-org-bookmark--buffer)

(defun counsel-org-bookmark--go-deeper (record)
  "Find an descendant heading from an Org bookmark RECORD."
  (let-alist
      (save-window-excursion
        (org-with-wide-buffer
         (bookmark-handle-bookmark record)
         (let (heading subtree-end)
           (if (org-before-first-heading-p)
               (progn
                 (goto-char (point-min))
                 (setq heading (buffer-name)
                       subtree-end (point-max)))
             (org-back-to-heading)
             (setq heading (org-get-heading t t t t)
                   subtree-end (save-excursion (org-end-of-subtree))))
           (setq counsel-org-bookmark--buffer (current-buffer))
           `((heading . ,heading)
             (candidates
              . ,(let (result)
                   (while (re-search-forward org-heading-regexp subtree-end t)
                     (push (cons (string-trim-right (thing-at-point 'line t))
                                 (save-excursion (beginning-of-line) (point)))
                           result))
                   (nreverse result)))))))
    (ivy-read (format "Headings in %s: " .heading)
              .candidates
              :action
              '(1
                ("o" (lambda (cell)
                       (with-ivy-window
                         (switch-to-buffer counsel-org-bookmark--buffer)
                         (goto-char (cdr cell))
                         (org-show-entry)))
                 "jump")
                ("j" (lambda (cell)
                       (switch-to-buffer-other-window counsel-org-bookmark--buffer)
                       (goto-char (cdr cell))
                       (org-show-entry))
                 "other-window")
                ("c" (lambda (cell)
                       (with-ivy-window
                         (switch-to-buffer counsel-org-bookmark--buffer)
                         (goto-char (cdr cell))
                         (counsel-org-bookmark--create-child)))
                 "create a child")))))

(provide 'counsel-org-bookmark)
;;; counsel-org-bookmark.el ends here
