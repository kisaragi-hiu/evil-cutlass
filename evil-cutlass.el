;;; evil-cutlass.el --- Make evil deletion operators actually just delete -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu <mail@kisragi-hiu.com>
;; URL: https://github.com/kisaragi-hiu/evil-cutlass
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
  "Cutlass.vim for Emacs."
  :prefix "evil-cutlass-"
  :group 'evil)

(defcustom evil-cutlass-lighter
  " Cutlass"
  "String used on the mode-line."
  :group 'evil-cutlass
  :type 'string)

;; A manually collected stack of evil commands that end up calling evil-yank:
;;
;; (legend:
;;  the star: these are ones we want to override
;;  the colon: this function is called by something else, listed below
;;  the period: this function is not directly called by anything else)
;;
;; evil-yank:
;;   evil-yank-line.
;;   evil-delete(*):
;;     evil-change(*):
;;       evil-substitute(*).
;;       evil-change-line(*).
;;       evil-change-whole-line(*):
;;         evil-change-line(*).
;;     evil-delete-line(*).
;;     evil-delete-whole-line(*).
;;     evil-delete-char(*).
;;     evil-delete-backward-char(*).
;;     evil-visual-paste:
;;       evil-paste-before.
;;       evil-paste-after.
;;
;; This means we can simply override evil-delete.

(defun evil-cutlass--redirect-to-blackhole-advice (orig beg end &optional type register yank-handler)
  "Advice to make `evil-delete' use the blackhole register by default.
ORIG is the original function.
BEG, END, TYPE, REGISTER, and YANK-HANDLER are `evil-delete' arguments."
  (unless (or register (eq this-command 'evil-visual-paste))
    (setq register ?_))
  (funcall orig beg end type register yank-handler))

;;;###autoload
(defalias 'evil-cutlass-cut (symbol-function 'evil-delete)
  "Cut text like the original `evil-delete' does.")

;;;###autoload
(defalias 'evil-cutlass-cut-line (symbol-function 'evil-delete-line)
  "Cut to end of line like the original `evil-delete-line' does.")

;;;###autoload
(define-minor-mode evil-cutlass-mode
  "Create a distinction between cut and delete for Evil."
  :lighter " Cutlass"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (evil-define-key* 'normal map "x" #'evil-cutlass-cut)
            (evil-define-key* 'normal map "X" #'evil-cutlass-cut-line)
            map)
  :require 'evil-cutlass
  (if evil-cutlass-mode
      (advice-add #'evil-delete :around #'evil-cutlass--redirect-to-blackhole-advice)
    (advice-remove #'evil-delete #'evil-cutlass--redirect-to-blackhole-advice)))

(provide 'evil-cutlass)

;;; evil-cutlass.el ends here
