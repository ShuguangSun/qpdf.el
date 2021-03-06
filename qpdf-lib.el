;;; qpdf-lib.el --- qpdf common stuff                -*- lexical-binding: t; -*-

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

;; Common stuffs used in multiple files.

;;; Code:


(require 'compile)
(require 'transient)

(defgroup qpdf nil
  "Qpdf with transient menu."
  :group 'qpdf
  :prefix "qpdf")

(defvar qpdf-option-boolean-list '("--decrypt" "--verbose" "--progress")
  "Qpdf BOOLEAN options.")

(defvar qpdf-option-string-list '("--password"  "--password-file")
  "Qpdf STRING options.")





(provide 'qpdf-lib)
;; Local variables:
;; package-lint-main-file: "qpdf.el"
;; end:
;;; qpdf-lib.el ends here
