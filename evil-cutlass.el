;;; evil-cutlass.el --- Make evil deletion operators actually just delete -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu <mail@kisragi-hiu.com>
;; URL:
;; Package-Requires: ((emacs "24.4") (evil "1.0.0"))
;; Version: 0.2.0
;; Keywords: emulations, evil, vim, convenience

;; This file is NOT part of GNU Emacs.

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
;;
;; Modify evil's change and deletion operators to just delete without
;; modifying the default register.
;;
;; `evil-use-register' can still be used as usual.
;;
;;; Code:

(require 'evil)

(defgroup evil-cutlass nil
  "Cutlass.vim for Emacs"
  :prefix "evil-cutlass-"
  :group 'evil)

(defcustom evil-cutlass-lighter
  " Cutlass"
  "String used on the mode-line."
  :group 'evil-cutlass
  :type 'string)

(defun evil-cutlass--third-advice (cmd &rest args)
  "Use this as an advice like this:

  (advice-add 'evil-change-whole-line :around #'evil-cutlass--third-advice)

This will then make `evil-change' use the black hole register by default.

The \"third\" in the name refers to the fact that `evil-change'
accepts REGISTER as the fourth argument. This has to be done as
`evil-this-register' is read in the interactive declaration,
which is run before advices, so setting it in an advice like this
will not have any effect. For commands that accept REGISTER as
the fourth argument, use `evil-cutlass--fourth-advice' instead.

CMD is the original function. ARGS is the argument list."
  ;; If `evil-this-register' is nil, that means the user did not
  ;; specify another register, so we swap out the register argument
  ;; with the black hole register. It isn't cleared until the end of
  ;; the command, allowing us to still see its value at this point.
  (unless evil-this-register
    (setq args (list (elt args 0)
                     (elt args 1)
                     ?_
                     ;; nthcdr = -drop
                     (nthcdr 3 args))))
  (apply cmd args))

(defun evil-cutlass--fourth-advice (cmd &rest args)
  "Use this as an advice like this:

  (advice-add 'evil-change :around #'evil-cutlass--fourth-advice)

This will then make `evil-change' use the black hole register by default.

The \"fourth\" in the name refers to the fact that `evil-change'
accepts REGISTER as the fourth argument. This has to be done as
`evil-this-register' is read in the interactive declaration,
which is run before advices, so setting it in an advice like this
will not have any effect. For commands that accept REGISTER as
the third argument, use `evil-cutlass--third-advice' instead.

CMD is the original function. ARGS is the argument list."
  ;; If `evil-this-register' is nil, that means the user did not
  ;; specify another register, so we swap out the register argument
  ;; with the black hole register. It isn't cleared until the end of
  ;; the command, allowing us to still see its value at this point.
  (unless evil-this-register
    (setf (elt args 3) ?_))
  (apply cmd args))

(defvar evil-cutlass--commands
  '((evil-change                 :advice evil-cutlass--fourth-advice)
    (evil-change-line            :advice evil-cutlass--fourth-advice)
    (evil-substitute             :advice evil-cutlass--fourth-advice)
    (evil-change-whole-line      :advice evil-cutlass--third-advice)
    (evil-org-delete             :advice evil-cutlass--fourth-advice)
    (evil-delete                 :advice evil-cutlass--fourth-advice)
    (evil-delete-line            :advice evil-cutlass--fourth-advice)
    (evil-delete-char            :advice evil-cutlass--fourth-advice)
    (evil-delete-backward-char   :advice evil-cutlass--fourth-advice)))

;;;###autoload
(defalias 'evil-cutlass-cut (symbol-function 'evil-delete))

;;;###autoload
(define-minor-mode evil-cutlass-mode
  "Create a distinction between cut and delete for Evil."
  :lighter " Cutlass"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key* 'normal map "x" #'evil-cutlass-cut)
            map)
  :require 'evil-cutlass
  (if evil-cutlass-mode
      (progn
        ;; update any possible changes to `evil-delete'
        (fset 'evil-cutlass-cut (symbol-function 'evil-delete))
        ;; add advice
        (dolist (command-cfg evil-cutlass--commands)
          (let ((cmd (car command-cfg))
                (advice (plist-get (cdr command-cfg) :advice)))
            (advice-add cmd :around advice))))
    ;; remove advice
    (dolist (command-cfg evil-cutlass--commands)
      (let ((cmd (car command-cfg))
            (advice (plist-get (cdr command-cfg) :advice)))
        (advice-remove cmd advice)))))

(provide 'evil-cutlass)

;;; evil-cutlass.el ends here
