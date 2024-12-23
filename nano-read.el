;;; nano-read.el --- NANO read                       -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicolas P. Rougier (inria)

;; Author: Nicolas P. Rougier (inria) <nicolas.rougier@inria.fr>
;; URL: https://github.com/rougier/nano-read
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: minbuffer convenience

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

;; nano-read functions allows the user to enter text in the minibuffer
;; while some information is displayed on the right side (right
;; margin). This information can be static, dynamic and/or modifiable
;; via some key bindings, depending on the nano-read function.
;;
;;  - nano-read:             This displays nothing
;;  - nano-read-with-info:   This displays a static information
;;  - nano-read-with-result: This displays a live evaluation
;;  - nano-read-with-date:   This displays a modifiable date
;;  - nano-read-with-list:   This displays a modifiable selection
;;  - nano-read-yes-or-no:   This asks the user a yes or no answer
;;
;; The prompt can be displayed in the margin depending on the value of
;; the `prompt-in-margin' parameter.

;;; Example usage:
;;
;: (nano-read "PROMPT")
;; (nano-read-with-info "NOTE" (propertize "JOURNAL" 'face 'nano-faded))
;; (nano-read-with-date "MEETING")
;; (nano-read-with-result "CALC" #'calc-eval)
;; (nano-read-yes-or-no "WARNING" "Buffer modified, kill it?")
;: (nano-read-with-list "REPLY" '("ALL" "SENDER"))
;; (nano-read-with-list "COMMIT" '("REGION" "FILE" "ALL"))
;; (nano-read-with-list "TODO"
;;                      '("WORK" "HOME" "TALK" "TRIP"  "MAIL")
;;                      "Something to do at work")

;;; NEWS:
;;
;;  Version 0.1
;;   - initial release

;;; Code:

(defface nano-read-face
  `((t :inherit widget-field))
  "Face for entry")

(defface nano-read-prompt-default-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'link nil t)
       :weight ,(face-attribute 'bold :weight)
       :box ,(face-foreground 'default)))
  "Face for default prompt")

(defface nano-read-prompt-warning-face
  `((t :foreground ,(face-background 'default)
       :background ,(face-foreground 'error nil t)
       :weight ,(face-attribute 'bold :weight)
       :box ,(face-foreground 'error nil t)))
  "Face for warning prompt")

(defvar nano-read-prompt-face 'nano-read-prompt-default-face
  "Prompt face")

(defvar nano-read-with-date-map
  (define-keymap
      :parent minibuffer-mode-map
    "<tab>"       #'nano-read-with-date--today
    "S-<right>"   #'nano-read-with-date--forward-day
    "S-<left>"    #'nano-read-with-date--backward-day
    "M-<right>"   #'nano-read-with-date--forward-week
    "M-<left>"    #'nano-read-with-date--backward-week
    "M-S-<right>" #'nano-read-with-date--forward-month
    "M-S-<left>"  #'nano-read-with-date--backward-month
    "S-<down>"    #'nano-read-with-date--backward-30mn
    "S-<up>"      #'nano-read-with-date--forward-30mn)
  "Keymap is used in conjunction with the `nano-read-with-date'
and allows to set the date and time on the right side.")

(defvar nano-read-with-list-map
  (define-keymap
      :parent minibuffer-mode-map
    "<tab>"    #'nano-read-with-list--next
    "S-<down>" #'nano-read-with-list--next
    "S-<up>"   #'nano-read-with-list--prev)
  "Keymap is used in conjunction with the `nano-read-with-list'
and allows to select next/prev item on the right side.")

(defvar nano-read-with-result-map
  (define-keymap
      :parent minibuffer-mode-map
    "<tab>"  #'nano-read-with-result--eval)
  "Keymap is used in conjunction with the `nano-read-with-result'
and allows to eval current expression.")

(defvar nano-read-yes-or-no-map
  (define-keymap
      :parent minibuffer-mode-map
      "<tab>"   #'nano-read-with-list--next
      "<down>"  #'nano-read-with-list--next
      "<up>"    #'nano-read-with-list--prev
      "<right>" #'nano-read-with-list--next
      "<left>"  #'nano-read-with-list--prev
      "y"       #'nano-read-yes-or-no--yes
      "n"       #'nano-read-yes-or-no--no)
  "Keymap is used in conjunction with the `nano-read-yes-or-no'
and allows to select YES or NO on the right side.")

(defun nano-read--format-date (date)
  "Format DATE for minibuffer right margin."

  (concat (propertize (format-time-string "%a %d %b %Y, " date)
                      'face `(:weight ,(face-attribute 'default :weight)))
          (propertize (format-time-string "%H:%M" date)
                      'face `(:weight ,(face-attribute 'bold :weight)))))

(defun nano-read--format-list (list)
  "Format LIST for minibuffer right margin."

  (nth (car list) (cdr list)))

(defun nano-read--format-info (info)
  "Format INFO for minibuffer right margin."

  info)

(defun nano-read-with-result--eval ()
  "Evaluate current minibuffer content"
  (interactive)

  (let ((content (buffer-substring-no-properties (length (minibuffer-prompt))
                                                 (point-max))))
    (when (functionp nano-read--eval)
      (condition-case nil
          (let ((result (funcall nano-read--eval content)))
            (setq nano-read--result (if (stringp result) result "..."))
            (setcdr nano-read--info (if (stringp result) result "..."))
            )
        (setcdr nano-read--info "Error")))
    (nano-read--update)))

(defun nano-read-with-list--next ()
  "Select next item in current list"

  (interactive)
  (let* ((list (cdr nano-read--info))
         (index (car list))
         (items (cdr list))
         (index (mod (1+ index) (length items))))
  (setcdr nano-read--info (cons index items)))
  (nano-read--update))

(defun nano-read-with-list--prev ()
  "Select previous item in current list"

  (interactive)
  (let* ((list (cdr nano-read--info))
         (index (car list))
         (items (cdr list))
         (index (mod (1- index) (length items))))
    (setcdr nano-read--info (cons index items)))
  (nano-read--update))

(defun nano-read-yes-or-no--yes ()
  "Select YES answer (if present in the current list)"

  (interactive)
  (let ((list (cdr nano-read--info)))
    (when (or (member "YES" (cdr list))
              (member "Yes" (cdr list))
              (member "yes" (cdr list)))
      (setcdr nano-read--info (cons 0 '("YES"))))
    (exit-minibuffer)))

(defun nano-read-yes-or-no--no ()
  "Select NO answer (if present in the current list)"

  (interactive)
  (let ((list (cdr nano-read--info)))
    (when (or (member "NO" (cdr list))
              (member "No" (cdr list))
              (member "no" (cdr list)))
      (setcdr nano-read--info (cons 0 '("NO"))))
    (exit-minibuffer)))

(defun nano-read-with-date--set-date (minutes hours days months years)
  "Add MINUTES minutes, HOURS hours, DAYS day, MONTH months & YEARS
years to DATE and display it."

  (let* ((date (cdr nano-read--info))
         (date (decode-time date))
         (date (encode-time 0
                           (+ minutes (nth 1 date))
                           (+ hours (nth 2 date))
                           (+ days (nth 3 date))
                           (+ months (nth 4 date))
                           (+ years (nth 5 date)))))
    (setcdr nano-read--info date)
    (nano-read--update)))

(defun nano-read-with-date--today ()
  (interactive)
  (let* ((date (decode-time (current-time))))
    (setf (nth 1 date) 0)
    (setcdr nano-read--info (encode-time date))
    (nano-read--update)))

(defun nano-read-with-date--forward-30mn ()
  (interactive)
  (nano-read-with-date--set-date +30 0 0 0 0))

(defun nano-read-with-date--backward-30mn ()
  (interactive)
  (nano-read-with-date--set-date  -30 0 0 0 0))

(defun nano-read-with-date--forward-hour ()
  (interactive)
  (nano-read-with-date--set-date 0 +1 0 0 0))

(defun nano-read-with-date--backward-hour ()
  (interactive)
  (nano-read-with-date--set-date  0 -1 0 0 0))

(defun nano-read-with-date--forward-day ()
  (interactive)
  (nano-read-with-date--set-date 0 0 +1 0 0))

(defun nano-read-with-date--backward-day ()
  (interactive)
  (nano-read-with-date--set-date 0 0 -1 0 0))

(defun nano-read-with-date--forward-week ()
  (interactive)
  (nano-read-with-date--set-date 0 0 +7 0 0))

(defun nano-read-with-date--backward-week ()
  (interactive)
  (nano-read-with-date--set-date 0 0 -7 0 0))

(defun nano-read-with-date--forward-month ()
  (interactive)
  (nano-read-with-date--set-date 0 0 0 +1 0))

(defun nano-read-with-date--backward-month ()
  (interactive)
  (nano-read-with-date--set-date 0 0 0 -1 0))

(defun nano-read-with-date--forward-year ()
  (interactive)
  (nano-read-with-date--set-date 0 0 0 0 +1))

(defun nano-read-with-date--backward-year ()
  (interactive)
  (nano-read-with-date--set-date 0 0 0 0 -1))

(defun nano-read--update ()
  "Update minibuffer right margin according to current mode;"

  (let* ((inhibit-read-only t)
         (formatter (car nano-read--info))
         (info      (cdr nano-read--info))
         (info      (funcall formatter info))
         (info      (if (stringp info) info ""))
        (prompt (substring (minibuffer-prompt) 2 (length (minibuffer-prompt)))))

    (add-text-properties (+ 0 (point-min)) (+ 1 (point-min))
                         `(display ((margin right-margin) ,info)))
    (if nano-read-prompt-in-margin
        (progn
          (set-window-margins nil (+ 1 (length prompt)) (+ 1 (length info)))
          (add-text-properties (+ 1 (point-min)) (+ 3 (length prompt))
                               `(display ((margin left-margin) ,prompt))))
      (progn
          (set-window-margins nil 0 (+ 1 (length info)))
          (add-text-properties (+ 1 (point-min)) (+ 2 (point-min))
                               `(display ((margin left-margin) " ")))))))

(defun nano-read--setup (&optional prompt-in-margin)
  "Setup minibuffer for nano-read"

  (setq truncate-lines -1)
  (visual-line-mode 1)
  (setq-local nano-read-prompt-in-margin prompt-in-margin)
  (make-local-variable 'face-remapping-alist)
  (add-to-list 'face-remapping-alist `(default nano-read-face))
  (nano-read--update))

(defun nano-read--stylize-prompt (prompt)
  "Stylize a prompt to add box around it"

  (propertize (concat (propertize " " 'display '(raise +0.15))
                      prompt
                      (propertize " " 'display '(raise -0.15)))
              'face nano-read-prompt-face))

(defun nano-read (prompt &optional default prompt-in-margin)
  "Read a string from the minibuffer, prompting with string PROMPT."


  (setq nano-read--info (cons #'identity ""))
  (minibuffer-with-setup-hook
      (:append (lambda () (nano-read--setup prompt-in-margin)))
    (let ((enable-recursive-minibuffers nil))
      (read-from-minibuffer (concat "  "
                                    (nano-read--stylize-prompt prompt)
                                    " ") default))))

(defun nano-read-with-result (prompt eval-fun &optional default prompt-in-margin)
  "Read a string from the minibuffer, prompting with string PROMPT while
displaying evaluation of the content using EVAL-FUN."

  (setq nano-read--info (cons #'nano-read--format-info
                              ""))
  (setq nano-read--eval eval-fun)
  (minibuffer-with-setup-hook
      (:append (lambda () (nano-read--setup prompt-in-margin)))
    (let ((enable-recursive-minibuffers nil))
      (add-hook 'post-command-hook #'nano-read-with-result--eval)
      (unwind-protect
          (read-from-minibuffer (concat "  "
                                        (nano-read--stylize-prompt prompt)
                                        " ") default nano-read-with-result-map)
        (remove-hook 'post-command-hook #'nano-read-with-result--eval))))
  (kill-new nano-read--result)
  nano-read--result)

(defun nano-read-with-info (prompt info &optional default prompt-in-margin)
  "Read a string from the minibuffer, prompting with string PROMPT while displaying INFO."

  (setq nano-read--info (cons #'nano-read--format-info
                              info))
  (minibuffer-with-setup-hook
      (:append (lambda () (nano-read--setup prompt-in-margin)))
    (let ((enable-recursive-minibuffers nil))
      (read-from-minibuffer (concat "  "
                                    (nano-read--stylize-prompt prompt)
                                    " ") default))))

(defun nano-read-with-date (prompt &optional date default prompt-in-margin)
  "Read a string and a date from the minibuffer, prompting with string
PROMPT."

  (let* ((date (decode-time (or date (current-time)))))
    (setf (nth 1 date) 0)
    (setq nano-read--info (cons #'nano-read--format-date
                                (encode-time date))))
  (minibuffer-with-setup-hook
      (:append (lambda () (nano-read--setup prompt-in-margin)))
    (let ((enable-recursive-minibuffers nil))
      (cons (read-from-minibuffer (concat "  "
                                          (nano-read--stylize-prompt prompt)
                                          " ")
                                  default nano-read-with-date-map)
            (cdr nano-read--info)))))

(defun nano-read-with-list (prompt list &optional default prompt-in-margin)
  "Read a string and a choice in LIST from the minibuffer, prompting with
string PROMPT."

  (setq nano-read--info (cons #'nano-read--format-list
                              (cons 0 list)))
  (minibuffer-with-setup-hook
      (:append (lambda () (nano-read--setup prompt-in-margin)))
    (let ((enable-recursive-minibuffers nil))
      (cons (read-from-minibuffer (concat "  "
                                          (nano-read--stylize-prompt prompt)
                                          " ")
                                  default nano-read-with-list-map)
            (let ((list (cdr nano-read--info)))
              (nth (car list) (cdr list)))))))

(defun nano-read-yes-or-no (prompt &optional default list prompt-in-margin)
  "Read a YES/NO answer from the minibuffer"

  (setq nano-read--info (cons #'nano-read--format-list
                              (cons 0 (or list (list "YES" "NO")))))
  (minibuffer-with-setup-hook
      (:append (lambda ()
                 (nano-read--setup prompt-in-margin)
                 (goto-char (point-min))
                 (setq cursor-type nil)))
    (define-key nano-read-yes-or-no-map [t] 'ignore)
    (define-key nano-read-yes-or-no-map (kbd "<return>") #'exit-minibuffer)
    (let ((history-add-new-input nil)
          (nano-read-prompt-face 'nano-read-prompt-warning-face)
          ;; (nano-read-info-face 'nano-strong)
          (enable-recursive-minibuffers nil))
      (read-from-minibuffer (concat "  "
                                    (nano-read--stylize-prompt prompt)
                                    " ")
                            default nano-read-yes-or-no-map))
    (let ((list (cdr nano-read--info)))
      (nth (car list) (cdr list)))))

(provide 'nano-read)
;;; nano-read.el ends here
