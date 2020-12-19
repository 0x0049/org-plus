;;; org+.el --- Org additions. -*- lexical-binding: t -*-

;; Copyright (c) 2020 0x0049

;; Author: 0x0049 <dev@0x0049.me>
;; URL: https://github.com/0x0049/org-plus
;; Keywords: org
;; Version: 1.0.0

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; org+ adds extra configuration options to org-mode.

;;; Code:

(require 'org)
(require 'org-clock)

(defgroup org+ nil
  "Org-mode enhancements."
  :group 'org)

(defcustom org+-mks-expert nil
  "Non-nil means use an expert selection scheme with `\\[org-mks]'.

No special window with the keyword will be shown and choices will
only be listed in the prompt."
  :group 'org+
  :type 'boolean)

(defcustom org+-prevent-duplicate-state-changes nil
  "Non-nil means to prevent duplicate state changes."
  :group 'org+
  :type 'boolean)

(defcustom org+-start-states '("START")
  "List of start states.

The first will be used as the default start state."
  :group 'org+
  :type '(repeat string))

(defcustom org+-babel-map
  '((bash . shell)
    (elisp . emacs-lisp)
    (javascript . js))
  "Alist of language aliases."
  :group 'org+
  :type 'alist)

(defun org+--load-language (fn &optional arg info params)
  "Load language if not already done then call FN with ARG, INFO, and PARAMS."
  (let* ((info (or info (org-babel-get-src-block-info)))
         (actual-language (intern (car info)))
         (language (or (alist-get actual-language org+-babel-map) actual-language)))
    (unless (alist-get language org-babel-load-languages)
      (add-to-list 'org-babel-load-languages (cons language t))
      (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
    (setcar info (symbol-name language))
    (funcall fn arg info params)))

(advice-add 'org-babel-execute-src-block :around #'org+--load-language)

(defun org+--prevent-duplicate-states (change-plist)
  "Return nil if the :from and :to properties of CHANGE-PLIST match and `org+-prevent-duplicate-state-changes' is non-nil."
  (cond ((and org+-prevent-duplicate-state-changes
              (eq (plist-get change-plist :type) 'todo-state-change)
              (equal (plist-get change-plist :from) (plist-get change-plist :to)))
         (setq org-block-entry-blocking "no change")
         nil)
        (t t)))

(add-hook 'org-blocker-hook #'org+--prevent-duplicate-states)

;;;###autoload
(defun org+-maybe-goto-capture-file ()
  "If not in an org file move to the default notes file, otherwise stay put.

This is meant to be used with capture templates so you can
capture to the current file."
  (unless (derived-mode-p 'org-mode)
    (set-buffer (org-capture-target-buffer org-default-notes-file))))

(defun org+--clock-in-switch-to-state (state)
  "Switch to a start state if STATE is not already a start state."
  (when (and state (not (seq-contains-p org+-start-states state))) (car org+-start-states)))

(setq org-clock-in-switch-to-state #'org+--clock-in-switch-to-state)

(defun org+--mks (table title &optional prompt specials)
  "Select a member of an alist with multiple keys.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"...

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let* ((inhibit-quit t)
           (buffer (unless org+-mks-expert (org-switch-to-buffer-other-window "*Org Select*")))
           (prompt (or prompt "Select: "))
           (body nil)
           (special-body nil)
           case-fold-search
           current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq body nil)
              (when buffer
                (erase-buffer)
                (insert title "\n\n"))
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (setq body (concat
                                     (if body (concat body " ") "{")
                                     "[" k "] ..." desc "..."))
                         (when buffer (insert prefix "[" k "]" "..." "  " desc "..." "\n"))))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (setq body (concat
                                     (if body (concat body " ") "{")
                                     "[" k "] " desc))
                         (when buffer (insert prefix "[" k "]" "     " desc "\n"))
                         (push k allowed-keys)))
                      (_ nil))))
                (when body (setq body (concat body "}")))
                ;; Insert special entries, if any.
                (setq special-body nil)
                (when specials
                  (when buffer (insert "----------------------------------------------------\
---------------------------\n"))
                  (pcase-dolist (`(,key ,description) specials)
                    (setq special-body
                          (concat
                           (if special-body (concat special-body " ") "{")
                           "[" key "] " description))
                    (when buffer (insert (format "[%s]     %s\n" key description)))
                    (push key allowed-keys))
                  (when special-body (setq special-body (concat special-body "}"))))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (when buffer
                  (goto-char (point-min))
                  (unless (pos-visible-in-window-p (point-max))
                    (org-fit-window-to-buffer)))
                (let ((pressed
                       (org--mks-read-key
                        allowed-keys
                        (if buffer
                            prompt
                          (concat "[a-z...]:Set\n" body special-body))
                        (when buffer
                          (not (pos-visible-in-window-p (1- (point-max))))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))

(advice-add 'org-mks :override #'org+--mks)

(defun org+--agenda-to-appt ()
  "Generate appointments from the agenda."
  (org-agenda-to-appt t)
  (appt-check))

(defun org+--agenda-to-appt-with-timer ()
  "Generate appointments after a timer.

If `timer+' is not loaded run immediately instead."
  (if (fboundp 'timer+-idle-debounce)
      (timer+-idle-debounce #'org+--agenda-to-appt)
    (org+--agenda-to-appt)))

(defun org+--appt-schedule ()
  "Generate appointments after a timer if the current file is an agenda file.

If `timer+' is not loaded run immediately instead."
  (when (member (buffer-file-name) (org-agenda-files))
    (org+--agenda-to-appt-with-timer)))

;; Regenerate appointments when the day changes.
(run-at-time "00:01" (* 60 60 24) #'org+--agenda-to-appt-with-timer)

(defun org+--hook-appt-schedule ()
  "Regenerate appointments when saving an org file."
  (add-hook 'before-save-hook #'org+--appt-schedule t t))

(add-hook 'org-mode-hook #'org+--hook-appt-schedule)

(defvar org+--log-current-level nil)
(defun org+--log-set-current-level ()
  "Store the current entry level."
  (setq org+--log-current-level (org-current-level)))

(advice-add 'org-add-log-note :before #'org+--log-set-current-level)

(defun org+--log-adjust-fill-column ()
  "Adjust the fill column so the note will be correct wrapped when inserted."
  (let ((offset (if org-adapt-indentation (+ org+--log-current-level 3) 2)))
    (setq fill-column (- fill-column offset))))

(add-hook 'org-log-buffer-setup-hook #'org+--log-adjust-fill-column)

(provide 'org+)

;;; org+.el ends here
