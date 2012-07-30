(setq inhibit-splash-screen t)
;;; zeroein.el --- Zero setup Emacs IPython Notebook client

;; Copyright (C) 2012- Takafumi Arakaki

;; Author: Takafumi Arakaki

;; This file is NOT part of GNU Emacs.

;; zeroein.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; zeroein.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with zeroein.el.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;; load-path configurations
(defvar zeroein:root-dir "~/.emacs.d/")

(defun zeroein:path (p &rest ps)
  (if ps
      (apply #'zeroein:path
           (concat (file-name-as-directory p) (car ps)) (cdr ps))
    (concat zeroein:root-dir p)))

(mapc (lambda (path) (add-to-list 'load-path (zeroein:path path)))
      '("ein/lisp" "markdown-mode" "websocket" "python" "auto-complete"
        "popup" "fuzzy" "pos-tip" "smartrep"))

(load (zeroein:path "nxhtml" "autostart.el"))


;;; Configurations
(eval-when-compile (require 'ein-notebooklist))
(require 'ein)

;; auto-complete
(setq ein:use-auto-complete-superpack t)
;; (setq ein:use-smartrep t)

(require 'auto-complete-config nil t)
(when (featurep 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories
               (zeroein:path "auto-complete" "dict"))
  (global-auto-complete-mode t))

;; MuMaMo
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background dark)) nil))))


;;; Workaround

;; Suppress this warning when using mumamo:
;; Warning: `font-lock-syntactic-keywords' is an obsolete variable (as of 24.1);
;;     use `syntax-propertize-function' instead.
(when (and (equal emacs-major-version 24)
           (equal emacs-minor-version 1))
  (eval-after-load "bytecomp"
    '(add-to-list 'byte-compile-not-obsolete-vars
                  'font-lock-syntactic-keywords)))
;; See: http://stackoverflow.com/a/5470584/727827

;;; zeroein.el ends here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-path (cons "~/.emacs.d/" load-path))

(setq load-path (cons "~/.emacs.d/color-theme-6.6.0/" load-path))
(setq load-path (cons "/Applications/Emacs.app/Contents/Resources/lisp/"
		      load-path))

(require 'php-mode)
(require 'counsyl)
(require 'flymake)
(require 'flymake-cursor)
(require 'coffee-mode)

(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 4)
  (define-key coffee-mode-map "\C-j" 'coffee-newline-and-indent))

(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))
(define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
(setq-default indent-tabs-mode nil)


(defun my-flymake-show-next-error()
    (interactive)
    (flymake-goto-next-error)
    )
(global-set-key "\C-c\C-j" 'my-flymake-show-next-error)




(setq py-python-command-args '("--colors=Linux"))

(require 'color-theme)
(color-theme-initialize)

(if window-system
    (color-theme-midnight))



;; One-True-Style window switching
(defun select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "M-s-<right>") 'select-next-window)
(global-set-key (kbd "M-s-<left>")  'select-previous-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;use codequality for type and style checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (load "flymake" t)
  (defun flymake-codequality-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
               'flymake-create-temp-inplace))
       (local-file (file-relative-name
            temp-file
            (file-name-directory buffer-file-name))))
      (list "codequality" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
           '("\\.py\\'" flymake-codequality-init))
  (add-to-list 'flymake-allowed-file-name-masks
           '("\\.js\\'" flymake-codequality-init))
  (add-hook 'find-file-hook 'flymake-find-file-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(column-number-mode t)
 '(ein:use-auto-complete t)
 '(ein:use-auto-complete-superpack nil)
 '(global-linum-mode t)
 '(show-paren-mode t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PERSONAL IDIOSYNCRACIES 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-u") 'delete-region)
;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
;; Scroll down with the cursor,move down the buffer one 
;; line at a time, instead of in larger amounts.
(setq scroll-step 1)
;; do not make backup files
(setq make-backup-files nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fullscreen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'maxframe)
    (add-hook 'window-setup-hook 'maximize-frame t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Surround word or region with html tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun tag-word-or-region (&optional start-reg end-reg tag)
    "Surround current word or region with a given tag."
    (interactive "r\nsEnter tag (without <>): ")
    (let (pos1 pos2 bds start-tag end-tag)
        (setq start-tag (concat "<" tag ">"))
        (setq end-tag (concat "</" (car (split-string tag " ")) ">"))
            (progn
                (goto-char end-reg)
                (insert end-tag)
                (goto-char start-reg)
                (insert start-tag))))

(global-set-key "\C-xt" 'tag-word-or-region)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG STATEMENTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'python-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d")
			   (lambda () (interactive)
			     (insert "import ipdb; ipdb.set_trace()")))
	    )
	  )
(add-hook 'js-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d") (lambda () (interactive)
					   (insert "debugger;")))
	    )
	  )
(add-hook 'coffee-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c d") (lambda ()
					   (interactive)
					   (insert "debugger;")))
	    )
	  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;AUTOPAIR:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'autopair)
(autopair-global-mode t)

(add-hook 'python-mode-hook
          #'(lambda ()
              (push '(?' . ?')
                    (getf autopair-extra-pairs :code))
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

;; TODO shell launch jiggery-pokery, fix autocompleteion in notebook