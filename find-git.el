;;; find-git.el --- git working tree browser

;; Copyright (C) 2010  

;; Author:  <lieutar@1dk.jp>
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


;;; basic options
(defvar find-git-status-function 'magit-status)
(defvar find-git-nested-tree-list ())
(defvar find-git-exclude-pathes-list ())
(defvar find-git-exclude-patterns-list ())

(defvar find-git-include-patterns-list ())
(defvar find-git-include-pathes-list ())

;;; internal variables
(defconst find-git-buffers-alist ())






;;; utilities
(defun find-git--make-pattern (patterns-list
                               pathes-list)
  (mapconcat
   #'identity
   (append
    patterns-list
    (mapcar (lambda (x)
              (format "\\`%s/?\\'"(regexp-quote
                                   (expand-file-name x))))
            pathes-list)
    )
   "\\|")
)

(defun find-git--include-pattern ()
  (or (and (or find-git-include-patterns-list
               find-git-include-pathes-list)
           (find-git--make-pattern find-git-include-patterns-list
                                   find-git-include-pathes-list))
      "\\`\0\\'")
  )


(defun find-git--exclude-pattern ()
  (find-git--make-pattern (append (list "/\\.git\\'")
                                  find-git-exclude-patterns-list)
                          find-git-exclude-pathes-list)
  )

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

(defun find-git--add-text-propeties-to-line (line props)
  (let (buffer-read-only)
    (save-excursion
      (goto-line line)
      (beginning-of-line)
      (add-text-properties (point) (progn (condition-case nil
                                              (next-line)
                                            (error (end-of-line)))  (point))
                           props))))


;;;
;;; find-git-mode
;;;


(defconst find-git-mode-repos-mark "    ")
(defconst find-git-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<up>")   'find-git-mode-previous-line)
    (define-key km (kbd "k")      'find-git-mode-previous-line)
    (define-key km (kbd "<down>") 'find-git-mode-next-line)
    (define-key km (kbd "j")      'find-git-mode-next-line)
    (define-key km (kbd "d")      'find-git-mode-dired)
    (define-key km (kbd "s")      'find-git-mode-git-status)
    (define-key km (kbd "q")       'bury-buffer)
    km))

;;; buffer local variables
(defvar find-git-base-directory nil)
(defvar find-git-current-line   nil)
(defvar find-git-current-repos  nil)

;;; faces
(dolist (spec
         '((find-git-title-face
            :weight      ultra-bold
            :background  unspecified
            :foreground  unspecified
            :underline   t)
           (find-git-repos-face)
           (find-git-current-repos-face
            :background "#CFC")
           ))
  (let* ((face  (car spec))
         (props (cdr spec)))
    (make-face face)
    (while props
      (let ((prop (car props))
            (val  (cadr props)))
        (setq props (cddr props))
        (set-face-attribute face nil prop val)))))

(defun find-git-mode--repos-at-point (point)
  (let ((ln (line-number-at-pos point)))
    (when (> ln 1)
      (let ((dir (save-excursion
                   (goto-char point)
                   (beginning-of-line)
                   (re-search-forward
                    (regexp-quote find-git-mode-repos-mark) nil t)
                   (buffer-substring-no-properties
                    (point) (progn (end-of-line) (point))))))
        (when (and (> (length dir) 0)
                   (file-directory-p dir))
          dir)))))

(defun find-git-mode--after-moved ()
  (when (and find-git-current-line
             (> find-git-current-line 1))
    (find-git--add-text-propeties-to-line
     find-git-current-line
     '(face find-git-repos-face)))
  (setq find-git-current-line (line-number-at-pos (point)))
  (let ((repo (find-git-mode--repos-at-point (point))))
    (when repo
      (find-git--add-text-propeties-to-line
       (line-number-at-pos (point)) '(face find-git-current-repos-face))
      (setq find-git-current-repos repo))))
  

;;; find-git-mode-commands
(defun find-git-mode-git-status ()
  (interactive)
  (let ((repo (find-git-mode--repos-at-point (point))))
    (when repo
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
                           buf)))))
         (win (selected-window)))
        (pop-to-buffer buf)
        (select-window win)))))


(defun find-git-mode-previous-line (&optional n)
  (interactive)
  (previous-line n)
  (find-git-mode--after-moved))

(defun find-git-mode-next-line (&optional n)
  (interactive)
  (next-line n)
  (find-git-mode--after-moved))

(defun find-git-mode-dired ()
  (interactive)
  (let ((repo (find-git-mode--repos-at-point (point))))
    (when repo
      (let ((win (selected-window))
            (buf (save-window-excursion
                   (dired repo)
                   (current-buffer))))
        (pop-to-buffer buf)
        (select-window win)))))


(define-derived-mode find-git-mode text-mode "find-git"
  (mapcar
   'make-variable-buffer-local
   '(find-git-base-directory
     find-git-current-line
     find-git-current-repos)))


;;; commands
(defun find-git (base)
  (interactive (list (read-directory-name "base: ")))
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
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (format "git repositories under the %s.\n" base))
      (find-git--add-text-propeties-to-line
       1 '(face find-git-title-face))
      (find-git-mode)
      (setq find-git-base-directory (expand-file-name base)))

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

    (when buf
      (setq buffer-read-only t)
      (goto-char (point-min)))

    (reverse R)))


(provide 'find-git)
;;; find-git.el ends here
