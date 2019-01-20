;;; ibuffer-auto-groups.el --- Make ibuffer groups automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords:

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

;;

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'a)
(require 'dash)
(require 'f)

;;;; Variables

(defvar ibuffer-saved-filter-groups)

;;;; Customization

(defcustom ibuffer-auto-groups-directories
  '("~/src/emacs"
    "~/src"
    "~/.homesick/repos/main/home/")
  :type '(repeat directory))

(defcustom ibuffer-auto-groups-default-groups
  '(;; ("~/.homesick/repos/main"
    ;;  (directory . "/home/me/\\.homesick/repos/main/"))
    ("Emacs source"
     (directory . "/usr/share/emacs/"))
    ("Web"
     (derived-mode . w3m-mode))
    ("/tmp"
     (directory . "/tmp/"))
    ("~/.emacs.d"
     (visiting-file)
     (derived-mode . emacs-lisp-mode)
     (directory . "/home/me/\\.homesick/repos/emacs\\.d/"))
    ("comint"
     (derived-mode . comint-mode))
    ("customize"
     (derived-mode . Custom-mode))
    ("help/info"
     (or
      (derived-mode . Info-mode)
      (derived-mode . help-mode)))
    ("Magit"
     (derived-mode . magit-mode))
    ("Helm"
     (derived-mode . helm-major-mode))
    ("*special*"
     (starred-name))
    ("~/org"
     (directory . "/home/me/org/")))
  :type '(alist :key-type string
                ;; MAYBE: Add detail to cons.
                :value-type (cons)))

;;;; Commands


;;;; Functions

(defun ibuffer-auto-groups-set-groups ()
  "Set \"Auto-groups\" saved groups in `ibuffer-saved-filter-groups'."
  (let ((directory-groups (->> (-map #'ibuffer-auto-groups-directory
                                     ibuffer-auto-groups-directories)
                               (-flatten-n 1))))
    (setf ibuffer-saved-filter-groups
          (a-assoc ibuffer-saved-filter-groups
                   "Auto-groups" (append directory-groups
                                         ibuffer-auto-groups-default-groups)))))

(cl-defun ibuffer-auto-groups-directory (directory &optional (depth 1))
  "Return groups for directories DEPTH levels beneath DIRECTORY."
  (let ((directories (list directory)))
    (dotimes (i depth)
      (setf directories (-flatten (-map #'f-directories directories))))
    (--map (cons (f-relative it default-directory)
                 (a-list 'directory (regexp-quote it)))
           directories)))

;;;; Footer

(provide 'ibuffer-auto-groups)

;;; ibuffer-auto-groups.el ends here
