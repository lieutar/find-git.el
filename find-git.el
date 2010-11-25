;;; find-git.el --- git working tree browser

;; Copyright (C) 2010  

;; Author:  <lieutar@TREEFROG>
;; Keywords: 

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; 

;;; Code:
(require 'cl)

(defvar find-git-status-function 'magit-status)
(defvar find-git-nested-tree-list ())
(defvar find-git-exclude-pathes-list ())
(defvar find-git-exclude-patterns-list ())

(defvar find-git-include-patterns-list ())
(defvar find-git-include-pathes-list ())

(defvar find-git--visited-dir ())

(defun find-git--make-pattern (patterns-list
                               pathes-list)
  (mapconcat
   #'identity
   (append
    patterns-list
    (mapcar (lambda (x)
              (format "\\`%s\\'"(regexp-quote
                                 (expand-file-name x))))
            pathes-list)
    )
   "\\|"))

(defun find-git--include-pattern ()
  (or (and (or find-git-include-patterns-list
               find-git-include-pathes-list)
           (find-git--make-pattern find-git-include-patterns-list
                                   find-git-include-pathes-list))
      "\\`\0\\'"))


(defun find-git--exclude-pattern ()
  (find-git--make-pattern (append (list "/\\.git\\'")
                                  find-git-exclude-patterns-list)
                          find-git-exclude-pathes-list))

(defun find-git--nested-tree-pattern ()
  (find-git--make-pattern ()
                          find-git-nested-tree-list)
  )

(defun find-git--walk-dir (base cb)
  (unless (eq :stop (apply cb (list base)))
    (dolist (sub (directory-files (expand-file-name base)))
      (let ((full (expand-file-name (concat base "/" sub))))
        (when (and (not (string-match "\\`\\.\\.?\\'" sub))
                   (file-directory-p full)
                   (file-readable-p full))
          (find-git--walk-dir full cb))))))


(defconst find-git-buffers-alist ())
(defconst find-git-mode-repos-mark "    ")

(defun find-git-mode--after-moved ()
  (let ((ln (line-number-at-pos (point))))
    (when (> ln 1)
      (let ((repo (save-excursion
                    (beginning-of-line)
                    (re-search-forward
                     (regexp-quote find-git-mode-repos-mark) nil t)
                    (buffer-substring-no-properties
                     (point) (progn (end-of-line) (point))))))

        (when (file-directory-p repo)
          (let* ((slot (assoc repo find-git-buffers-alist))
                 (buf  (if (and slot
                                (buffer-live-p (cdr slot)))
                           (cdr slot)
                         (progn
                           (save-window-excursion
                             (apply
                              find-git-status-function
                              (list (concat repo "/")))
                             (let ((buf (current-buffer)))
                               (setq find-git-buffers-alist
                                     (cons
                                      (cons repo buf)
                                      find-git-buffers-alist))
                               buf))))))
            (let ((win (selected-window)))
              (pop-to-buffer buf)
              (select-window win))))))))
  
(defun find-git-mode-previous-line (&optional n)
  (interactive)
  (previous-line n)
  (find-git-mode--after-moved))

(defun find-git-mode-next-line (&optional n)
  (interactive)
  (next-line n)
  (find-git-mode--after-moved))

(defconst find-git-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<up>")   'find-git-mode-previous-line)
    (define-key km (kbd "k")      'find-git-mode-previous-line)
    (define-key km (kbd "<down>") 'find-git-mode-next-line)
    (define-key km (kbd "j")      'find-git-mode-next-line)
    km))

(define-derived-mode find-git-mode text-mode "find-git")



(defun find-git (base)
  (interactive (list (read-directory-name "base: ")))
  (setq find-git-buffers-alist ())
  (let ((xpat (find-git--exclude-pattern))
        (ipat (find-git--include-pattern))
        (npat (find-git--nested-tree-pattern))
        (echo (if (interactive-p)
                  (lambda (path)
                    (insert (format "%s%s\n" find-git-mode-repos-mark path))
                    )
                (lambda (x))))
        (buf  (when (interactive-p)
                (get-buffer-create
                 (format "*find-git %s*"
                         (expand-file-name base)))))
        (R    ()))

    (when buf
      (pop-to-buffer buf)
      (set-buffer    buf)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (format "git repositories under the %s.\n" base))
      (find-git-mode))

    (find-git--walk-dir
     (expand-file-name base)
     (lambda (path)
       (condition-case nil
           (message (replace-regexp-in-string "%" "%%" path))
         (error))
       (cond ((and (string-match xpat path)
                   (not (string-match ipat path))) :stop)
             ((file-directory-p (expand-file-name (concat path "/.git")))
              (apply echo (list path))
              (setq R (cons path R))
              (unless (string-match npat path) :stop)))))
    (reverse R)))


(provide 'find-git)
;;; find-git.el ends here
