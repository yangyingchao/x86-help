;;; x86-help.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;  Helper to show description for assembly instructions.

;;  Usage:
;;  Add this file into load-path, and add following codes into init.el:
;; (autoload 'x86-help "x86-help" "show help for x86 assembly instructions." t)
;; (autoload 'x86-help-new-ref "x86-help" "create new reference." t)
;; (global-set-key (kbd "C-h x") #'x86-help)

;;; Code:
(require 'helm-source)
(yc/autoload 'eww)

(defcustom x86-help-reference-dir "~/Documents/TechBooks/x86-help/refs"
  "Folder to put references.."
  :type 'string
  :group 'x86-help)

(defvar x86-help--candidates nil "List of candidates.")

(defun x86-help--candidates ()
  "Generate a list of candidates."
  (unless (file-exists-p x86-help-reference-dir)
    (mkdir x86-help-reference-dir t))
  (unless x86-help--candidates
    (setq x86-help--candidates
          (mapcar
           (lambda (x)
             (cons (upcase (file-name-sans-extension (file-name-nondirectory x))) x))
           (directory-files x86-help-reference-dir t (rx (+ alnum))))))
  (unless x86-help--candidates
    (error "Nothing found in %s.
Download it from: http://www.felixcloutier.com/x86/ and decompress it to: %s."
           x86-help-reference-dir x86-help-reference-dir))
  x86-help--candidates)

(defun x86-help--show (path)
  "Show PATH with either emacs or eww."
  (let* ((ext (file-name-extension path)))
    (cond
     ((string= ext "html")   (eww (format "file://%s" path)))
     (t (find-file path)))))

;;;###autoload
(defun x86-help-new-ref (name)
  "Create an new reference for NAME."
  (interactive "sReference Name: ")
  (defun x86-help--save-hook ()
    "hook to run after snippet saved"
    (interactive)
    (setq x86-help--candidates nil)
    (x86-help--candidates)
    (remove-hook 'after-save-hook 'x86-help--save-hook))
  (let* ((dirname (expand-file-name x86-help-reference-dir))
         (filename (concat dirname "/" (upcase name) ".org")))
    (unless (file-directory-p dirname) (mkdir dirname t))
    (add-hook 'after-save-hook 'x86-help--save-hook)
    (find-file filename)
    (erase-buffer)
    (insert (format "* NAME\n %s - \n\n" (upcase name)))
    (insert "* SYNOPSIS\n\n* DESCRIPTION\n\n* EXAMPLES\n\n")
    ))

;;;###autoload
(defun x86-help ()
  "Look up reference of an x86 assembly instruction."
  (interactive)
  (let ((x-source
         (helm-build-sync-source "Help-X86"
           :init nil
           :candidates (x86-help--candidates)
           :persistent-action 'x86-help--show
           :action  (helm-make-actions
                    "Open File" 'x86-help--show
                    "Edit File" 'find-file)
           )))
    (helm :sources 'x-source
          :buffer "*helm*"
          :preselect (thing-at-point 'symbol))
    ))

(provide 'x86-help)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; x86-help.el ends here
