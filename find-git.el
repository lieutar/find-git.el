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
(defvar find-git-nested-tree-list      ())
(defvar find-git-exclude-pathes-list   ())
(defvar find-git-exclude-patterns-list ())

(defvar find-git-include-patterns-list ())
(defvar find-git-include-pathes-list   ())

;;; internal variables
(defconst find-git-buffers-alist       ())

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

(defun find-git--add-text-properties-to-line (line props)
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


(defconst find-git-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "<up>")   'find-git-mode-previous-line)
    (define-key km (kbd "k")      'find-git-mode-previous-line)
    (define-key km (kbd "<down>") 'find-git-mode-next-line)
    (define-key km (kbd "j")      'find-git-mode-next-line)
    (define-key km (kbd "d")      'find-git-mode-dired)
    (define-key km (kbd "s")      'find-git-mode-git-status)
    (define-key km (kbd "<RET>")  'find-git-mode-git-status)
    (define-key km (kbd "q")      'bury-buffer)
    km))

;;; buffer local variables
(defvar find-git-base-directory nil)
(defvar find-git-current-line   nil)
(defvar find-git-current-repos  nil)

;;; faces
(dolist (spec
         '((find-git-title-face
            :weight      ultra-bold
            :background  "#FC3"
            :foreground  unspecified
            :underline   t)
           (find-git-repos-face
            :foreground  "#00F")
           (find-git-repos+face
            :foreground  "#F00")
           (find-git-repos-face*
            :background "#CFC"
            :inherit    find-git-repos-face)
           (find-git-repos+face*
            :background "#CFC"
            :inherit    find-git-repos+face)
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
  (get-text-property point 'find-git-repos))

(defun find-git-mode--after-moved ()
  (when (and find-git-current-line
             (> find-git-current-line 1))
    (let* ((pre-face (save-excursion
                       (goto-line find-git-current-line)
                       (get-text-property (point) 'face)))
           (new-face (and pre-face
                          (intern (replace-regexp-in-string
                                   "\\*\\'" "" (face-name pre-face))))))
      (when pre-face
        (find-git--add-text-properties-to-line
         find-git-current-line
         `(face ,new-face)))))
  (setq find-git-current-line (line-number-at-pos (point)))
  (let ((repo (find-git-mode--repos-at-point (point))))
    (when repo
      (let* ((pre-face (get-text-property (point) 'face))
             (new-face (and pre-face
                            (intern (format "%s*" (face-name pre-face))))))
        (when pre-face
          (find-git--add-text-properties-to-line
           (line-number-at-pos (point)) `(face ,new-face)))
        (setq find-git-current-repos repo))
      (let ((slot (assoc repo find-git-buffers-alist)))
        (when slot (let ((win (selected-window)))
                     (pop-to-buffer (cdr slot))
                     (select-window win)))))))
  

;;; find-git-mode-commands
(defun find-git-mode-git-status ()
  (interactive)
  (let ((repo (find-git-mode--repos-at-point (point))))
    (when repo
      (let* ((slot (assoc repo find-git-buffers-alist))
             (buf  (if (and slot
                            (buffer-live-p (cdr slot)))
                       (cdr slot)
                     (let ((buf (save-window-excursion
                                  (apply
                                   find-git-status-function
                                   (list (concat repo "/")))
                                  (current-buffer))))
                       
                       (setq find-git-buffers-alist
                             (cons
                              (cons repo buf)
                              find-git-buffers-alist))
                       (find-git--add-text-properties-to-line
                        (line-number-at-pos (point))
                        '(face find-git-repos+face*))
                       buf)))
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

(defconst find-git-scanning-log nil)
;;; commands
(defun find-git (base)
  (interactive (list (read-directory-name "base: ")))
  (setq find-git-scanning-log nil)
  (let*
      ((base (expand-file-name
              (replace-regexp-in-string "[/\\\\]\\'" "" base)))
       (trunc-base-pattern (concat "\\`" (regexp-quote base)))
       (xpat (find-git--exclude-pattern))
       (ipat (find-git--include-pattern))
       (npat (find-git--nested-tree-pattern))
       (echo (if (interactive-p)
                 (lambda (path)
                   (insert
                    (format ".%s\n" 
                            (replace-regexp-in-string
                             trunc-base-pattern
                             ""
                             path)
                            ))
                   (previous-line)
                   (find-git--add-text-properties-to-line
                    (line-number-at-pos (point))
                    `(
                      find-git-repos
                      ,path
                      face
                      ,(if (assoc path find-git-buffers-alist) 
                           'find-git-repos+face
                         `find-git-repos-face)))
                   (next-line)
                   )
               (lambda (x))))
       (buf  (when (interactive-p)
               (get-buffer-create (format "*find-git %s*" base))))
       (R    ()))

    (when buf
      (pop-to-buffer buf)
      (set-buffer    buf)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (format "git repositories under the %s.\n" base))
      (find-git--add-text-properties-to-line
       1 '(face find-git-title-face))
      (find-git-mode)
      (setq find-git-base-directory base))

    (find-git--walk-dir
     base
     (lambda (path)
       (condition-case nil
           (message (replace-regexp-in-string "%" "%%" path))
         (error))
       (let ((xmatch (string-match xpat path))
             (imatch (string-match ipat path))
             (nmatch (string-match npat path))
             (gitp   (file-directory-p (expand-file-name ".git" path))))

         (add-to-list 'find-git-scanning-log
                      (list :path   path
                            :xmatch xmatch
                            :imatch imatch
                            :nmatch nmatch
                            :gitp   gitp))

         (cond ((and xmatch (not imatch)) :stop)
               (gitp
                (apply echo (list path))
                (setq R (cons path R))
                (add-to-list 'find-git-scanning-log (list :MATCH! path))
                (unless nmatch :stop))))))

    (when buf
      (setq buffer-read-only t)
      (goto-char (point-min)))

    (reverse R)))



(provide 'find-git)
;;; find-git.el ends here
