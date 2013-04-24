;;; chep-tag-popup.el --- tag information in a popup inside emacs
;;
;; Filename: chep-tag-popup.el
;; Description: tag information in a popup inside emacs
;; Author: Cédric Chépied <cedric.chepied@gmail.com>
;; Maintainer: Cédric Chépied
;; Copyright (C) 2013, Cédric Chépied
;; Last updated: Sat March 23 00:07 UTC
;;     By Cédric Chépied
;;     Update 1
;; Keywords: tag popup
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; tag information in a popup inside emacs
;; If a tag table is loaded with visit-tag-table, chep-get-definition-etags
;; function will display in a popup the definition of the symbol at point.
;; Only tested for C language.
;;
;; You need popup.el: https://github.com/auto-complete/popup-el
;; 
;; To install copy chep-tag-popup.el or chep-tag-popup.elc inside emacs
;; lisp directory (global or personal) and add this to you emacs config:
;; (require 'chep-tag-popup)
;;
;; Then you can add a shortcut to chep-get-definition-etags
;; Global:
;; (global-set-key (kbd "C-M-.") 'chep-get-definition-etags)
;; Local to C mode:
;; (add-hook 'c-mode-hook '(lambda () (local-set-key (kbd "C-M-.") 'chep-get-definition-etags)))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;; Copyright Cédric Chépied 2013
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'etags)
(require 'popup)

(provide 'chep-tag-popup)

(defconst chep-keyword "\\(const\\|extern\\|static\\)")
(defconst chep-standard-types "\\(\\(unsigned\\|char\\|int\\|long\\|float\\|double\\|void\\|short\\)[ \t\n]*\\)")
(defconst chep-custom-type "\\(union\\|struct\\)?[ \t\n]*\\([a-zA-Z_][a-zA-Z0-9_]*[ \t\n]*\\)")
(defconst chep-var-name (concat "\\*?[ \t\n]*"
								chep-custom-type
								"[ \t\n]*\\(=[ \t\n]*[^;]*\\)?[ \t\n]*"))

(defun chep-var-at-point (element)
  "Returns the text of a variable declaration"
  (save-excursion
	(let ((start (point)) (ret ""))
	  (if (looking-back (concat "\\(" chep-keyword "[ \t\n]*\\)+"))
		  (setq ret (match-string 0)))
	  (if (looking-at (concat "[ \t\n]*\\(" chep-keyword "[ \t\n]*\\)*"
							  "\\(" chep-standard-types "+\\|" chep-custom-type "\\)"
							  "\\(" chep-var-name ",[ \t\n]\\)*"
							  "\\*?[ \t\n]*" element))
		  (progn (search-forward-regexp (concat "[^a-zA-Z_0-9]"
												element
												"[^a-zA-Z_0-9]"))
				 (backward-char)
				 (if (looking-at (concat "[ \t\n]*\\(\\[.*\\]\\)?"
										 "[ \t\n]*"
										 "\\(=[ \t\n]*[^;]*\\)?;"))
					 (progn (search-forward ";")
							(concat ret (buffer-substring start (point))))
				     nil))
		  nil))))

(defun chep-struct-at-point (element)
  "Returns the text of a struct/class/union/enum declaration"
  (save-excursion
	(end-of-defun)
	(beginning-of-defun)
	(let (start)
	  (setq start (point))
	  (if (not (looking-at (concat "^[ \n\t]*\\(typedef\\)?[ \n\t]*"
								   "\\(class\\|union\\|struct\\|\\enum\\)")))
		  nil
	      (progn (search-forward "{")
				 (backward-char)
				 (forward-sexp)
				 (search-forward ";")
				 (buffer-substring start (point)))))))


(defun chep-typedef-at-point (element)
  "Returns the text of a typedef declaration"
  (save-excursion
	(if (looking-at (concat "^[ \n\t]*\\(typedef\\)[ \n\t]*"
							"\\(" chep-standard-types "+\\|" chep-custom-type "\\)"
							element "[ \n\t]*;"))
		(match-string 0)
	    nil)))


(defun chep-func-at-point (element)
  "Returns the text of a function declaration"
  (save-excursion
	(end-of-defun)
	(beginning-of-defun)
	(let (start ret)
	  (setq start (point))
	  (re-search-forward "[;{]" nil t)
	  (setq ret (buffer-substring start (1- (point))))
	  (if (string= "" ret)
		  nil
		  ret))))

(defun chep-macro-at-point (element)
  "Return the text of a macro declaration"
  (save-excursion
	(if (looking-at (concat "^[ \t\n]*#define[ \t\n]*"
							element
							"\\(.*\\\\\n\\)*.*\n"))
		(match-string 0)
	    nil)))
	  

(defun chep-get-definition-etags ()
  (interactive)
  (let (result)
	(save-excursion (let (search buf )
					  ;;Recherche de l'élément au point
					  (setq search (funcall (or find-tag-default-function
												(get major-mode 'find-tag-default-function)
												'find-tag-default)))
					  ;;Recherche du tag et selection du buffer
					  (setq buf (find-tag-noselect search))
					  (set-buffer buf)
					  
					  ;; Récupération de la déclaration
					  (setq result (chep-var-at-point search))
					  (if (not result)
						  (progn (setq result (chep-macro-at-point search))
								 (if (not result)
									 (progn (setq result (chep-typedef-at-point search))
											(if (not result)
												(progn (setq result (chep-struct-at-point search))
													   (if (not result)
														   (progn (setq result (chep-func-at-point search))))))))))
					  ;;Retour 
					  (pop-tag-mark)))

	;;Affichage dans un popup
	(if result
		(popup-tip result)))
)

;; chep-tag-popup.el ends here
