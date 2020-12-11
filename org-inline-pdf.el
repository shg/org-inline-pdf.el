;;; org-inline-pdf.el --- Inline PDF previewing for Org -*- lexical-binding: t -*-

;; Copyright (C) 2020 Shigeaki Nishina

;; Author: Shigeaki Nishina
;; Maintainer: Shigeaki Nishina
;; Created: November 30, 2020
;; URL: https://github.com/shg/org-inline-pdf.el
;; Package-Requires: ((emacs "25.1") (org "9.4"))
;; Version: 0.2
;; Keywords: org, outlines, hypermedia

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:

;; Enable inline PDF preview in Org buffers.

;;; Usage:

;; You need to have pdf2svg command installed in exec-path.
;;
;; pdf2svg: https://cityinthesky.co.uk/opensource/pdf2svg/
;;
;; Download org-inline-pdf.el and install it using package.el
;;
;;   (package-install-file "/path-to-download-dir/org-inline-pdf.el")
;;
;; Require it in your init file.
;;
;;   (require 'org-inline-pdf)
;;
;; Now image links in Org buffers are displayed inline.
;;
;; Also, when the file is exported to HTML using ox-html, PDF will be
;; embedded using img tag. Note that PDF with img tag is not standard
;; and will be rendered only in particular browsers.  Safari.app is
;; only the one I know.
;;
;;; Code:

(eval-when-compile
  (require 'org))
(require 'ox-html)

(defvar org-inline-pdf-make-preview-program "pdf2svg")

(defun org-inline-pdf--make-preview-for-pdf (original-org--create-inline-image &rest arguments)
  "Make a SVG preview when the inline image is a PDF.
This function is to be used as an `around' advice to
`org--create-inline-image'.  The original function is passed in
ORIGINAL-ORG--CREATE-INLINE-IMAGE and arguments in ARGUMENTS."
  (let ((file (car arguments)))
    (apply original-org--create-inline-image
	   (cons
	    (if (member (file-name-extension file) '("pdf" "PDF"))
		(let ((svg (org-babel-temp-file "org-inline-pdf-")))
		  (call-process org-inline-pdf-make-preview-program nil nil nil file svg)
		  svg)
	      file)
	    (cdr arguments)))))

(add-to-list 'image-file-name-extensions "pdf")
(advice-add #'org--create-inline-image :around #'org-inline-pdf--make-preview-for-pdf)

;; Add .pdf to the image types embeded inline in exported html
;; files. The other types are copied from ox-html.el and need to be
;; updated if the original code is changed.
(setf (alist-get "file" org-html-inline-image-rules nil t 'string=)
      (regexp-opt '(".jpeg" ".jpg" ".png" ".gif" ".svg" ".pdf")))

(provide 'org-inline-pdf)

;;; org-inline-pdf.el ends here
