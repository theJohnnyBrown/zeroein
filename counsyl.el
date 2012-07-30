;;; Grep

(defvar counsyl/grep-command
  (concat "find %s "
          "\\( -path \\*/.git -o -path \\*/migrations -o -path \\*/build -o -path \\*thirdparty \\) -prune "
          "-o -type f \\( -iname \\*.py -o -iname \\*.coffee -o -iname \\*.html -o -iname \\*.css -o -iname \\*.el \\) "
          "-print0 | xargs -P2 -0 grep -i -nH -e %s"))


(defun counsyl/grep (&optional arg)
  (interactive "P")
  (compile
   (format
    counsyl/grep-command
    (counsyl--git-dir)
    (if arg (read-from-minibuffer "Regexp: ")
      (or (thing-at-point 'symbol) (error "No word at point"))))))


;;; Github

(defun counsyl/open-in-github (&optional clipboard-only)
  "Open current file location in github.
With C-u prefix argument copy URL to clipboard only."
  (interactive "P")
  (let ((git-dir (counsyl--git-dir))
        (branch (counsyl--git-current-branch))
        (line (line-number-at-pos (point))))
    (if git-dir
        (if branch
            (let* ((repo (file-name-nondirectory
                          (directory-file-name git-dir)))
                   (path (replace-regexp-in-string
                          (concat "^" git-dir) "" (buffer-file-name)))
                   (url (format
                         "https://github.counsyl.com/dev/%s/blob/%s%s#L%d"
                         repo branch path line)))
              (if clipboard-only
                  (progn (kill-new url) (message url))
                (browse-url url)))
          (message "Not on a branch"))
      (message "Not in a git repo"))))


;;; Python

(defun counsyl/python-import (&optional arg)
  "Form python import statements

  Default is to prompt for a name and insert the corresponding
  import statement. With C-u prefix arg, form the import
  statement for the name at point and copy to kill ring."
  (interactive "P")
  (if arg (counsyl--python-import-name-at-point)
    (counsyl--python-import-name)))


(defun counsyl--python-import-name ()
  "Inserts the import statement given a website.git variable name."
  (let* ((stdout
	  (org-babel-chomp
	   (shell-command-to-string
	    (format
	     "find ~/website/counsyl/product -name '*py' -type f | xargs grep -hi '^from .* import %s$' | sort | uniq"
	     (read-from-minibuffer "Variable Name: ")))))
	 (lines (when (> (length stdout) 0)
		  (split-string stdout "\n"))))
    (if lines
	(insert
	 (concat
	  (if (eq (length lines) 1)
	      (car lines)
	    (completing-read "TAB to complete: " lines nil t "from ")) "\n"))
      (message "No matching import lines")
      nil)))


(defun counsyl--python-import-name-at-point ()
  "Form import line for name at point and copy to kill ring"
  (let ((name (thing-at-point 'symbol)))
    (if (not name)
	(message "No name found at point")
      (let* ((path
	      (replace-regexp-in-string
	       (concat "^" (counsyl--git-dir) "/") "" (buffer-file-name)))
	     (import
	      (format "from %s import %s"
		      (replace-regexp-in-string
		       "/" "."
		       (replace-regexp-in-string "\\.py$" "" path)) name)))
	(message import)
	(counsyl--kill-ring-save-string import)))))


;;; Key bindings

;; Example:
;;
;; Add code like the following to your .emacs file to specify key
;; bindings in different major modes:
;;
;; (counsyl/register-key-bindings
;;  '(global-map .
;;               (("\C-c\M-g" . counsyl/grep))))
;; (require 'python)
;; (counsyl/register-key-bindings
;;  '("python" .
;;    (("\C-ci" . counsyl/python-import)
;;     ([f5] . counsyl/open-in-github))))
;;
;; More examples of key sequences:
;; [delete], [(super o)], [(control next)], [f4], [(meta up)]
;; super is OS X cmd key; binding some modifier keys may not be
;; possible if you are using emacs in a terminal.
;; See http://www.gnu.org/software/emacs/manual/html_node/emacs/Init-Rebinding.html

(defvar counsyl/key-bindings nil
  "List of all key bindings.
This is an alist of alists. The key of the top level alist
references a key map. If the key is a string, the string
\"-mode-map\" is appended to it when finding the mode-map. If it
is a symbol, it is used as is.")


(defun counsyl/register-key-bindings (bindings-alist)
  "Add bindings in counsyl/key-bindings"
  (setq
   counsyl/key-bindings
   (cons
    bindings-alist
    (counsyl--assoc-delete-all (car bindings-alist) counsyl/key-bindings)))
  (counsyl--set-key-bindings)
  nil)


;;; Utilities

(defun counsyl--set-key-bindings (&optional mode-map in-mode-map)
  "Set custom key bindings
Optional argument MODE-MAP sets bindings in that mode only
Optional argument IN-MODE-MAP sets MODE-MAP bindings in IN-MODE-MAP
"
  (interactive)
  (mapc (lambda (pair)
          (let* ((map (or in-mode-map (car pair)))
                 (bindings (cdr pair)))
            (if (stringp map) (setq map (intern (concat map "-mode-map"))))
            (if (symbolp map) (setq map (eval map)))
            (mapc (lambda (binding)
                    (define-key map (car binding) (cdr binding)))
                  bindings)))
        (or (and mode-map `(,(assoc mode-map counsyl/key-bindings)))
            counsyl/key-bindings)))


(defun counsyl--assoc-delete-all (key alist)
  "Like `assq-delete-all' but using `equal' for comparison"
  (delq nil
        (mapcar (lambda (el) (unless (equal (car el) key) el))
                alist)))


(defun counsyl--git-dir ()
  "Root dir of current repo"
  (let ((git-dir (org-babel-chomp
		  (shell-command-to-string "git rev-parse --git-dir"))))
    (directory-file-name
     (if (equal git-dir ".git")
	 default-directory
       (file-name-directory git-dir)))))


(defun counsyl--git-current-branch ()
  ((lambda (stdout)
     (and (> (length stdout) 0)
          (replace-regexp-in-string "^refs/heads/\\(.+\\)\n" "\\1" stdout)))
   (shell-command-to-string "git symbolic-ref HEAD 2>/dev/null")))


(defun counsyl--kill-ring-save-string (string)
  "Save a string to the kill ring"
  (with-temp-buffer
    (insert string)
    (kill-ring-save (point-min) (point-max))))


;; In emacs24 but not emacs23
(defun org-babel-chomp (string &optional regexp)
  "Strip trailing spaces and carriage returns from STRING.
Default regexp used is \"[ \f\t\n\r\v]\" but can be
overwritten by specifying a regexp as a second argument."
  (let ((regexp (or regexp "[ \f\t\n\r\v]")))
    (while (and (> (length string) 0)
                (string-match regexp (substring string -1)))
      (setq string (substring string 0 -1)))
    string))


(provide 'counsyl)
