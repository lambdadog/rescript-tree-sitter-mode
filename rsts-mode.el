;;; rsts-mode.el --- tree-sitter-backed major mode for ReScript  -*- lexical-binding: t -*-

;; Copyright (C) 2022 Ashlynn Anderson

;; Author     : Ashlynn Anderson <contact@pea.sh>
;; Maintainer : Ashlynn Anderson <contact@pea.sh>
;; Created    : July 2022
;; Modified   : 2022
;; Version    : 0.1.0
;; Keywords   : rescript languages mode tree-sitter
;; X-URL      : https://github.com/lambdadog/rescript-tree-sitter-mode
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'tree-sitter)
(require 'tree-sitter-hl)

(defconst rsts-mode--dir
  (file-name-directory (or load-file-name buffer-file-name)))
(defconst rsts-mode--lang-dir
  (expand-file-name "lang/" rsts-mode--dir))
(defconst rsts-mode--lang-queries-dir
  (expand-file-name "queries/" rsts-mode--lang-dir))
(defconst rsts-mode--parser
  (expand-file-name "rsts-mode/parser/rescript.so" user-emacs-directory))

(defgroup rsts-mode nil
  "ReScript language mode using Tree Sitter."
  :group 'languages)

(defcustom rsts-cc "cc"
  "C compiler to use when building the tree-sitter parser."
  :type 'string
  :group 'rsts-mode)

(defcustom rsts-always-rebuild nil
  "Debug setting that causes rsts-mode to always rebuild the tree-sitter parser."
  :type 'boolean
  :group 'rsts-mode)

;; Doesn't do anything yet..
(defcustom rsts-indent-offset 2
  "Indent offset for rsts-mode."
  :type 'integer
  :group 'rsts-mode)

(defun rsts-mode--ensure-parser-then (cb)
  "Ensure parser exists, and if it doesn't then try to build it."
  (if (or rsts-always-rebuild
	  (not (file-exists-p rsts-mode--parser)))
      (progn
	(delete-file rsts-mode--parser)
	(when (y-or-n-p "ReScript tree-sitter parser must be built, do so now?")
	  (mkdir (file-name-directory rsts-mode--parser) 'with-parents)
	  (let ((default-directory rsts-mode--lang-dir))
	    (set-process-sentinel
	     (start-process "rescript-parser-cc" "*ReScript parser build*" rsts-cc
			    "-shared" "-fPIC" "-fno-exceptions" "-g" "-O2" "-Isrc"
			    "src/scanner.c" "src/parser.c" "-o" rsts-mode--parser)
	     (lambda (proc _event)
	       (when (eq 'exit (process-status proc))
		 (if (= 0 (process-exit-status proc))
		     (progn
		       (message "rsts-mode: build finished")
		       (funcall cb))
		   (pop-to-buffer "*ReScript parser build*")
		   (error "rsts-mode: build failed with exit code %d" (process-exit-status proc)))))))))
    (funcall cb)))

;;;###autoload
(define-derived-mode rsts-mode prog-mode "ReScript[Tree Sitter]"
  (setq-local tree-sitter-hl-default-patterns
	      (with-temp-buffer
		(insert-file-contents
		 (expand-file-name "highlights.scm" rsts-mode--lang-queries-dir))
		(buffer-string)))
  ;; Hack to make tree-sitter-hl-mode work
  (setq-local font-lock-defaults '(('t)))
  ;; TODO: indentation, take a look at
  ;; https://github.com/helix-editor/helix/pull/1562
  (setq-local indent-line-function #'indent-relative)
  (rsts-mode--ensure-parser-then
   (lambda ()
     (tree-sitter-mode 't)
     (tree-sitter-hl-mode 't))))

;;;###autoload
(progn
  (add-to-list 'tree-sitter-load-path (file-name-directory rsts-mode--parser))
  (add-to-list 'tree-sitter-major-mode-language-alist '(rsts-mode . rescript))
  (add-to-list 'auto-mode-alist '("\\.resi?\\'" . rsts-mode)))

(provide 'rsts-mode)
;;; rsts-mode.el ends here
