;;; ibuffer-auto-groups.el --- Make ibuffer groups automatically  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: ibuffer, buffers, convenience

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

;; Based on code posted at <https://www.reddit.com/r/emacs/comments/6qfvia/can_we_talk_about_managingswitching_between_many/dkz5v3n/>.

;;; Code:

;;;; Requirements

(require 'cl-lib)

(require 'a)
(require 'dash)
(require 'f)

;;;; Variables

(defvar ibuffer-saved-filter-groups)

;;;; Customization

(defgroup ibuffer-auto-groups nil
  "Automatically generate `ibuffer' groups."
  :group 'ibuffer)

(defcustom ibuffer-auto-groups-directories
  '(("~/src/emacs" . 1)
    ;; ("~/src" . 1)
    ("~/.homesick/repos/main/" . 2))
  "Directories to automatically generate groups for."
  :type '(repeat (cons directory depth)))

;; TODO: Var for modes to group automatically.

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
    ("~/org"
     (directory . "/home/me/org/"))
    ("*special*"
     (starred-name)))
  "Default groups added to \"Auto-groups\" groups, in format expected by `ibuffer-saved-filter-groups'."
  :type '(alist :key-type string
                ;; MAYBE: Add detail to cons.
                :value-type (cons)))

;;;; Commands

(define-minor-mode ibuffer-auto-groups-mode
  "Automatically generate \"Auto-groups\" in `ibuffer-saved-filter-groups'."
  :global t
  (if ibuffer-auto-groups-mode
      (advice-add #'ibuffer-update :before #'ibuffer-auto-groups-set-groups)
    (advice-remove #'ibuffer-update #'ibuffer-auto-groups-set-groups)))

;;;; Functions

(defun ibuffer-auto-groups-set-groups (&rest _ignore)
  "Set \"Auto-groups\" saved groups in `ibuffer-saved-filter-groups'."
  (let* ((auto-directory-groups (->> (--map (ibuffer-auto-groups-directory (car it) (cdr it))
                                            ibuffer-auto-groups-directories)
                                     (-flatten-n 1)))
         (auto-groups (append auto-directory-groups
                              ibuffer-auto-groups-default-groups
                              (ibuffer-auto-groups-buffer-groups))))
    (setf ibuffer-saved-filter-groups (a-assoc ibuffer-saved-filter-groups
                                               "Auto-groups" auto-groups)
          ibuffer-filter-groups auto-groups)))

(cl-defun ibuffer-auto-groups-directory (directory &optional (depth 1))
  "Return groups for directories DEPTH levels beneath DIRECTORY."
  (let ((directories (list directory)))
    (dotimes (_ depth)
      (setf directories (-flatten (-map #'f-directories directories))))
    (--map (cons (f-relative it default-directory)
                 (a-list 'auto-groups-directory (regexp-quote it)))
           directories)))

(defun ibuffer-auto-groups-buffer-groups ()
  "Return groups for directories for all buffers."
  (->> (buffer-list)
       (--map (let ((dir (or (buffer-file-name it)
                             (buffer-local-value 'default-directory it))))
                (if (f-directory? dir)
                    (directory-file-name dir)
                  (f-dirname dir))))
       (-uniq)
       (-non-nil)
       (--map (cons (f-relative it (or ibuffer-default-directory
                                       default-directory))
                    ;; MAYBE: Should we use `auto-groups-directory' here?
                    (a-list 'directory (rx-to-string `(seq ,it (or "/" eos))))))
       (-sort (-on #'string< #'car))))

;;;; ibuffer filter

(define-ibuffer-filter auto-groups-directory
    "Limit current view to buffers with directory matching QUALIFIER.

For a buffer associated with file '/a/b/c.d', this matches
against '/a/b'. For a buffer not associated with a file, this
matches against the value of `default-directory' in that buffer."
  (:description "ibuffer-auto-groups-directory name"
                :reader (read-from-minibuffer "Auto-Filter by directory name (regex): "))
  (when-let* ((dir (or (buffer-file-name buf)
                       ;; TODO: I guess we need a list of variables like `magit--default-directory' to check.
                       ;; Either that, or a white/blacklist of modes to check `default-directory' in.
                       (buffer-local-value 'magit--default-directory buf))))
    (string-match qualifier dir)))

;;;; Footer

(provide 'ibuffer-auto-groups)

;;; ibuffer-auto-groups.el ends here
