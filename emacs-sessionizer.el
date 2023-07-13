;;; emacs-sessionizer.el --- Manage multiple sessions in emacs easaly

;; Copyright (C) 2023 Jacob Stannix <jakestannix@gmail.com>

;; Author: Jacob Stannix <jakestannix@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ()
;; Keywords: projects, sessions

;;; Commentary:

;; This package is a rewrite of ThePrimeagen's tmux-sessionizer
;; (found here: https://github.com/ThePrimeagen/.dotfiles/blob/master/bin/.local/scripts/tmux-sessionizer)
;; to work in emacs

(provide 'emacs-sessionizer)

(define-minor-mode emacs-sessinoizer-mode
  "Toggle emacs-sessionizer-mode."
  :init-value nil)

(defgroup emacs-sessionizer nil
  "customizations for the package `emacs-sessionizer'")

(defcustom emacs-sessionizer-search-list '()
  "list of directories to search to populate selection list."
  :type 'list
  :require 'emacs-sessionizer-mode
  :group 'emacs-sessionizer)

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





