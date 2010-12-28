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
(defvar find-git-auto-status-mode      nil)

(defvar find-git-cache-file            "~/.emacs.d/find-git.cache")
(defvar find-git-status-function       'magit-status)
(defvar find-git-nested-tree-list      ())
(defvar find-git-exclude-pathes-list   ())
(defvar find-git-exclude-patterns-list ())

(defvar find-git-include-patterns-list ())
(defvar find-git-include-pathes-list   ())



;;; internal variables
(defconst find-git-cache                 ())
(defconst find-git-summary-buffers-alist ())
(defconst find-git-buffers-alist         ())
(defconst find-git-repos-alist           ())
(defconst find-git-anything-source       ())

(defsubst find-git-regularize (base)
  (expand-file-name
   (replace-regexp-in-string "[/\\\\]\\'" "" base)))

(defun find-git-load-cache ()
  (interactive)
  (when (file-exists-p find-git-cache-file)
    (let ((buf (find-file-noselect find-git-cache-file)))
      (save-excursion
        (set-buffer buf)
        (setq find-git-cache (read (buffer-substring-no-properties
                                    (point-min)(point-max))))
        (kill-buffer buf)
        t))))
(find-git-load-cache)

(defun find-git-save-cache ()
  (interactive)
  (when (file-writable-p find-git-cache-file)
    (let ((buf (find-file-noselect find-git-cache-file)))
      (save-excursion
        (set-buffer buf)
        (delete-region (point-min) (point-max))
        (insert (format "%S" find-git-cache))
        (kill-buffer buf)
        t))))

(defun find-git-get-cache (base)
  (let ((slot (assoc base find-git-cache)))
    (and slot (cdr slot))))

(defun find-git-put-cache (base repos-list)
  (setcdr (or (assoc base find-git-cache)
              (let ((slot (cons base nil)))
                (add-to-list 'find-git-cache slot)
                slot))
          repos-list)
  (find-git-save-cache))



;;; utilities
(defun find-git--put-alist (key value sym)
  (let* ((alist (symbol-value sym))
         (slot  (assoc key alist)))
    (set sym
         (if slot
             (progn
               (setcdr slot value)
               alist)
           (cons (cons key value) alist)))))


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
      "\\`\0\\'"))

(defun find-git--exclude-pattern ()
  (find-git--make-pattern (append (list "/\\.git\\'")
                                  find-git-exclude-patterns-list)
                          find-git-exclude-pathes-list))

(defun find-git--nested-tree-pattern ()
  (find-git--make-pattern ()
                          find-git-nested-tree-list))

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
      (add-text-properties
       (point) (progn (condition-case nil
                          (next-line)
                        (error (end-of-line)))  (point))
       props))))

(defun find-git-load-cache ()
  (interactive)
  (when (file-readable-p find-git-cache-file)
    (save-excursion 
      (let ((buf (find-file-noselect find-git-cache-file)))
        (set-buffer buf)
        (setq find-git-cache
              (read (buffer-substring-no-properties (point-min)
                                                    (point-max))))
        (kill-buffer buf)))))

(defun find-git-save-cache ()
  (interactive)
  (when (file-writable-p find-git-cache-file)
    (with-temp-buffer
      (insert (format "%S" find-git-cache))
      (setq buffer-file-name find-git-cache-file)
      (save-buffer))))

(find-git-load-cache)


;;;
;;; find-git-mode
;;;


(defconst find-git-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km (kbd "k")      'previous-line)
    (define-key km (kbd "j")      'next-line)
    (define-key km (kbd "d")       'find-git-mode-dired)
    (define-key km (kbd "s")       'find-git-mode-git-status)
    (define-key km (kbd "g")       'find-git-mode-refresh)
    (define-key km (kbd "<RET>")   'find-git-mode-git-status)
    (define-key km (kbd "C-x C-s") 'find-git-mode-save)
    (define-key km (kbd "q")      'bury-buffer)
    (define-key km (kbd "C-c a")  'find-git-auto-status-mode)
    km))

;;; buffer local variables
(defvar find-git-base-directory     nil)
(defvar find-git-current-line       nil)
(defvar find-git-current-repos      nil)
(defvar find-git-current-repos-list nil)
(defvar find-git-previous-window-configuration nil)
(defvar find-git-startup-window-configuration nil)

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

(defun find-git-mode--escape-current-window-configuration ()
  (unless find-git-previous-window-configuration
    (setq find-git-previous-window-configuration
          (current-window-configuration (selected-frame)))))

(defun find-git-mode--restore-previous-window-configuration ()
  (when find-git-previous-window-configuration
    (set-window-configuration
     find-git-previous-window-configuration)
    (setq find-git-previous-window-configuration nil)))

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
    (if repo
        (progn
          (let* ((pre-face (get-text-property (point) 'face))
                 (new-face (and pre-face
                                (intern (format "%s*" (face-name pre-face))))))
            (when pre-face
              (find-git--add-text-properties-to-line
               (line-number-at-pos (point)) `(face ,new-face)))
            (setq find-git-current-repos repo))
          (let ((slot (assoc repo find-git-buffers-alist)))
            (cond
             (slot
              (let ((win (selected-window)))
                (find-git-mode--escape-current-window-configuration)
                (pop-to-buffer (cdr slot))
                (select-window win)))
             (find-git-auto-status-mode
              (let ((win (selected-window)))
                (pop-to-buffer
                 (find-git-mode-git-status--internal repo))
                (select-window win)))
             (t
              (find-git-mode--restore-previous-window-configuration)))))
      (find-git-mode--restore-previous-window-configuration))))
  

;;; find-git-mode-commands
(defun find-git-mode-git-status--internal (repo)
  (find-git-mode--escape-current-window-configuration)
  (let ((buf (save-window-excursion
               (funcall find-git-status-function
                        (replace-regexp-in-string
                         "\\([^/]\\)\\'" "\\1/" repo))
               (current-buffer))))
    (setq find-git-buffers-alist (cons `(,repo . ,buf) find-git-buffers-alist))
    (find-git--add-text-properties-to-line
     (line-number-at-pos (point))
     '(face find-git-repos+face*))
    buf))

(defun find-git-mode-git-status ()
  (interactive)
  (let ((repo (find-git-mode--repos-at-point (point))))
    (when repo
      (let*
          ((slot (assoc repo find-git-buffers-alist))
           (buf  (if (and slot (buffer-live-p (cdr slot)))
                     (cdr slot)
                   (find-git-mode-git-status--internal repo)))
           (win (selected-window)))
        (pop-to-buffer buf)
        (select-window win)))))

(defun find-git-mode-save ()
  (interactive)
  (save-excursion
    (let ((base  find-git-base-directory)
          (repos find-git-current-repos-list))
      (with-temp-buffer
        (insert base "\n")
        (insert "----\n")
        (dolist (repo repos)
          (insert repo "\n"))
        (save-buffer)))))


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
   'make-local-variable
   '(find-git-base-directory
     find-git-current-line
     find-git-current-repos
     find-git-current-repos-list
     find-git-previous-window-configuration
     find-git-startup-window-configuration
     post-command-hook
     ))
  (add-hook 'post-command-hook 'find-git-mode--after-moved))


(defun find-git-mode-refresh ()
  (interactive)
  (find-git find-git-base-directory :refresh t))

(defun find-git-refresh ()
  (interactive)
  (let ((find-git-opts '(:refresh t)))
    (call-interactively 'find-git)))

(defconst find-git-scanning-log nil)


(defconst find-git-opts nil)

;;; commands

(defun find-git (base &rest opts)
  ""
  (interactive (cons (read-directory-name "find-git base: " "~/")
                     find-git-opts))
  (setq find-git-scanning-log nil)
  (let*
      ((popup (if (member :popup opts)
                  (plist-get opts :popup)
                (if (plist-get opts :popup-to-current-window)
                    'current-window
                  t)))
       (refreshp (plist-get opts :refresh))
       (make-buffer-p
        (or (or (eq popup t)
                (eq popup 'current-window)
                (eq popup 'hidden))))
       (base (find-git-regularize base))
       (trunc-base-pattern (concat "\\`" (regexp-quote base)))
       (xpat (find-git--exclude-pattern))
       (ipat (find-git--include-pattern))
       (npat (find-git--nested-tree-pattern))
       (echo (if make-buffer-p
                 (lambda (path)
                   (add-to-list 'find-git-current-repos-list path)
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
                   (next-line))
               (lambda (x))))
       (buf  (when make-buffer-p
               (get-buffer-create (format "*find-git %s*" base))))
       (R    (if refreshp () (find-git-get-cache base))))

    (when buf
      (set-buffer buf)
      (setcdr (or (assoc (expand-file-name base)
                         find-git-summary-buffers-alist)
                  (let ((slot (cons base nil)))
                    (add-to-list 'find-git-summary-buffers-alist slot)
                    slot))
              buf)
      (find-git-mode)
      (setq find-git-startup-window-configuration
            (current-window-configuration (selected-frame)))

      (case popup
        ((       t      ) (pop-to-buffer    buf))
        ((current-window) (switch-to-buffer buf))
        (t                nil))

      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (goto-char (point-min))
      (insert (format "git repositories under the %s.\n" base))
      (find-git--add-text-properties-to-line
       1 '(face find-git-title-face))
      (setq find-git-base-directory base))

    (if R
        (dolist (repo R)
          (apply echo (list repo)))

      (progn (find-git--walk-dir
              base
              (lambda (path)
                (condition-case nil
                    (message (replace-regexp-in-string "%" "%%" path))
                  (error))
                (let ((xmatch (string-match xpat path))
                      (imatch (string-match ipat path))
                      (nmatch (string-match npat path))
                      (gitp   (file-directory-p
                               (expand-file-name ".git" path))))
                  (add-to-list 'find-git-scanning-log
                               (list :path   path
                                     :xmatch xmatch
                                     :imatch imatch
                                     :nmatch nmatch
                                     :gitp   gitp))
                  
                  (cond ((and xmatch (not imatch)) :stop)
                        (gitp
                         (apply echo (list path))
                         (setq R (cons (find-git-regularize path) R))
                         (add-to-list 'find-git-scanning-log
                                      (list :MATCH! path))
                         (unless nmatch :stop))))))
             (setq R (reverse R))
             (find-git-put-cache base R)))

    (when buf
      (setq buffer-read-only t)
      (goto-char (point-min)))

    (dolist (repo (reverse R))
      (unless (assoc repo find-git-repos-alist)
        (setq find-git-repos-alist (cons (cons repo repo)
                                         (cons (cons (file-name-nondirectory
                                                      (replace-regexp-in-string
                                                       "/\\'" "" repo))
                                                     repo)
                                               find-git-repos-alist))))
      (unless (member repo find-git-anything-source)
        (setq find-git-anything-source 
              (cons repo find-git-anything-source))))

    R))


(define-minor-mode find-git-auto-status-mode
  ""
  nil
  nil)

(defun find-git--read-repos ()
  (cdr (assoc (car (completing-read-multiple "repo? "
                                             find-git-repos-alist))
              find-git-repos-alist)))

(defun find-git-find-repos (repos)
  (interactive (list (find-git--read-repos)))
    (dired repos))

(defun find-git-git-status (repos)
  (interactive (list (find-git--read-repos)))
  (switch-to-buffer
   (let ((slot (assoc repos find-git-buffers-alist)))
     (if slot (cdr slot)
       (find-git-mode-git-status--internal repos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; anything sources
;;

(defconst anything-c-source-find-git
  '((name       . "Git repositories")
    (candidates . find-git-anything-source)
    (action . (("Visit to the directory" . dired)
               ("Git Status"             . find-git-git-status)))))

(defconst anything-c-source-find-git-bases
  '((name       . "Cached find-git summaries")
    (candidates . (lambda ()
                    (mapcar 'car find-git-cache)))
    (action . (("Visit Summary" . (lambda (base)
                                    (find-git base :popup 'current-buffer)))
               ("Refresh" . (lambda (base)
                              (find-git base :refresh t :popup nil)))
               ("Refresh and visit" .
                (lambda (base) (find-git base
                                         :refresh t
                                         :popup 'current-buffer)))))))

(defconst anything-c-source-find-git-commands
  '((name       . "find-git commands")
    (candidates . (find-git))
    (type . command)))

(defvar find-git-anything-c-sources
  '(anything-c-source-find-git
    anything-c-source-find-git-bases
    anything-c-source-find-git-commands))

(defun find-git-anything ()
  (interactive)
  (anything :sources find-git-anything-c-sources))



(provide 'find-git)
;;; find-git.el ends here
