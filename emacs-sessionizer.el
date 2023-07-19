;;; emacs-sessionizer.el --- Manage multiple sessions in emacs easaly

;; Copyright (C) 2023 Jacob Stannix <jakestannix@gmail.com>

;; Author: Jacob Stannix <jakestannix@gmail.com>
;; Version: 0.0.2
;; Package-Requires: ((emacs "28.2") (perspective "2.18") (fzf "0.0.2")) 
;; Keywords: projects, sessions

;;; Commentary:

;; This package is a rewrite of ThePrimeagen's tmux-sessionizer
;; (found here: https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer)
;; to work in emacs

;;; Code:
(require 'perspective)
(require 'fzf)


;;; --- customization

;;;###autoload
(defgroup emacs-sessionizer 'nil
  "customizations for the package `emacs-sessionizer'"
  :group 'perspective-mode)

;;;###autoload
(defcustom emacs-sessionizer-search-list '()
  "list of directories to search to populate selection list."
  :type 'list
  :require 'emacs-sessionizer-mode
  :group 'emacs-sessionizer)

;;;###autoload 
(defcustom emacs-sessionizer-prefix-key (kbd "C-c s")
  "Prefix key for `emacs-sessionizer-mode-map'"
  :group 'emacs-sessionozer
  :type 'key-sequence)

;;; --- implementation

(defun emacs-sessionizer--filter-dir-list (list)
  "Filters non directories from a dir list"
  (let ((result '()))
    (dolist (val list)
      (when
	  (and
	   (nth 1 val)
	   (not
	    (or
	     (string-equal (file-name-base (car val)) ".")
	     (string-equal (file-name-base (car val)) ".."))))
	(setq result (cons (car val) result))))
    result))

(defun emacs-sessionizer--build-dir-list ()
  (let ((result-list '()))
    (dolist (dir emacs-sessionizer-search-list)
     (setq result-list (append (directory-files-and-attributes dir 't) result-list)))
    (emacs-sessionizer--filter-dir-list result-list)))

;;(defvar emacs-sessionizer--cur-session 'nil)


(defun emacs-sessionizer-fzf-callback (session-dir)
;;  (setq emacs-sessionizer--cur-session session-dir)
  (cd session-dir)
  (persp-switch session-dir)
  )
(defun emacs-sessionizer-switch-perspective ()
  (interactive)
  (fzf-with-entries (emacs-sessionizer--build-dir-list) 'emacs-sessionizer-fzf-callback)
)

(defvar emacs-sessionizer-mode-map (make-sparse-keymap))

(define-key emacs-sessionizer-mode-map emacs-sessionizer-prefix-key #'emacs-sessionizer-switch-perspective)

;; (defun emacs-sessionizer--persp-switch-hook-function ()
;;   (cd emacs-sessionizer--cur-session))

;;(add-hook 'persp-switch-hook #'emacs-sessionizer--persp-switch-hook-function)

;;;###autoload
(define-minor-mode emacs-sessionizer-mode
  "Toggle emacs-sessionizer-mode."
  :global 't
  :keymap emacs-sessionizer-mode-map
  (if emacs-sessionizer-mode
      (progn
	(customize-set-variable 'persp-mode-prefix-key (kbd "C-c p"))
	(persp-mode 1))
    (progn
      (persp-mode -1)))
  )


(provide 'emacs-sessionizer)
;;; emacs-sessionizer.el ends here
