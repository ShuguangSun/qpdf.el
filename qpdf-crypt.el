;;; qpdf-crypt.el --- Encrypt/decrypt PDF            -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shuguang Sun

;; Author: Shuguang Sun <shuguang79@qq.com>
;; Created: 2021/11/30
;; Version: 0.0.1
;; URL: https://github.com/ShuguangSun/pdftk.el
;; Package-Requires: ((emacs "25.1") (transient "0.3.0"))
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; To Encrypt/decrypt PDF files.

;;; Code:

(require 'qpdf-lib)


(defun qpdf-crypt-decrypt--assert (args)
  "Parse transient ARGS."
  (let ((infile
         (car (delq
               nil
               (mapcar (lambda (x)
                         (if (string-prefix-p "--infile=" x)
                             (substring x 9)))
                       args))))
        (outfile
         (car (delq
               nil
               (mapcar (lambda (x)
                         (if (string-prefix-p "--outfile=" x)
                             (substring x 10)))
                       args))))
        (strlist (concat
                  "\\`\\("
                  (mapconcat 'identity qpdf-option-string-list "\\|")
                  "\\)=\\(.+\\)"))
        ret)
    (unless outfile
      (setq outfile (file-name-concat (file-name-directory infile)
                                      (concat (file-name-base infile)
                                              "_decrypted.pdf"))))
    (unless (and (file-exists-p outfile)
                 (yes-or-no-p (concat outfile " exists.  Overwrite? ")))
        (setq outfile (read-file-name "Out file name: "
                                      (file-name-directory infile)
                                      (file-name-nondirectory outfile))))
    (setq infile (shell-quote-argument infile))
    (setq outfile (shell-quote-argument outfile))
    (setq ret (mapcar (lambda (x)
                        (if (or (string-match-p strlist x)
                                (member x qpdf-option-boolean-list))
                            x))
                      args))
    (delq nil (append ret `,(list infile outfile)))))


(defun qpdf-crypt-decrypt-file (&optional args)
  "Qpdf decrypt file.
Optional argument ARGS Transient option."
  (interactive (if current-prefix-arg
                   nil
                 (list (transient-args 'qpdf-crypt-decrypt))))
  (let* ((args (qpdf-crypt-decrypt--assert args))
         (passwd (car (delq
                       nil
                       (mapcar (lambda (x)
                                 (if (string-prefix-p "--password=" x) x))
                               args))))
         compile-command)
    (unless passwd
      (setq passwd (concat "--password=" (read-passwd "Password: "))))
    (setq compile-command (format (concat "qpdf --decrypt " passwd " %s")
                                          (mapconcat 'identity args " ")))
    (compile compile-command t)))


;;; * menu
;;;###autoload (autoload 'qpdf-crypt-decrypt "qpdf-crypt" nil t)
(transient-define-prefix qpdf-crypt-decrypt ()
  "Decrypt pdf files."
  ["Arguments"
   ("f" "infile name" "--infile=" transient-read-file)
   ("o" "outfile name" "--outfile=" transient-read-file)
   ("p" "password (Not necessary for this step)" "--password=" read-passwd)
   ("v" "verbose" "--verbose")
   ]
  [["Command"
    ("d" "decrypt" qpdf-crypt-decrypt-file)
    ]])



(provide 'qpdf-crypt)
;;; qpdf-crypt.el ends here
