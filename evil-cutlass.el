;;; evil-cutlass.el --- Make evil deletion operators actually just delete -*- lexical-binding: t -8 -*-

;; Author: Kisaragi Hiu <mail@kisragi-hiu.com>
;; URL:
;; Package-Requires: ((emacs "24.4") (evil "1.0.0"))
;; Version: 0.0.1
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
;; A register can still be provided with `evil-use-register' (the `"' key).
;;
;;; Code:

(require 'evil)

(defgroup evil-cutlass nil
  "Cutlass.vim for Emacs"
  :prefix "evil-cutlass-"
  :group 'evil)

(defun evil-cutlass-advice (cmd &rest _args)
  "Advice to set CMD's default register to the black hole register."
  (unless evil-this-register
    (setq evil-this-register ?_))
  (call-interactively cmd))

(defvar evil-cutlass--commands
  '((evil-delete                :switch evil-goggles-enable-delete                :advice evil-goggles--generic-blocking-advice)
    (evil-delete-line           :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
    (evil-org-delete            :switch evil-goggles-enable-delete                :advice evil-goggles--delete-line-advice)
    (evil-change                :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-change-line           :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)
    (evil-change-whole-line     :switch evil-goggles-enable-change                :advice evil-goggles--generic-blocking-advice)))

(defcustom evil-cutlass-lighter
  " Cutlass"
  "String used on the mode-line."
  :group 'evil-cutlass
  :type 'string)

;;;###autoload
(define-minor-mode evil-cutlass-mode
  "evil-goggles global minor mode."
  :lighter " Cutlass"
  :global t
  :require 'evil-cutlass
  (if evil-cutlass-mode
      (progn
        ;; add advice
        (dolist (command-cfg evil-cutlass--commands)
          (let ((cmd (car command-cfg))
                (advice (plist-get (cdr command-cfg) :advice))
                (switch (plist-get (cdr command-cfg) :switch))
                (after  (plist-get (cdr command-cfg) :after)))
            (when (symbol-value switch)
              (advice-add cmd (if after :after :before) advice)))))
    ;; remove advice
    (dolist (command-cfg evil-goggles--commands)
      (let ((cmd (car command-cfg))
            (advice (plist-get (cdr command-cfg) :advice)))
        (advice-remove cmd advice)))))

(provide 'evil-cutlass)

;;; evil-cutlass.el ends here
