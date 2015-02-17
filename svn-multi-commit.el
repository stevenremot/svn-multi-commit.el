;;; svn-multi-commit.el --- Simple command to handle commit on multiple SVN branches at the same time

;; Author: Steven Rémot <steven.remot@gmail.com>
;; Version: 0.2.0
;; Keywords: svn
;; Homepage: https://github.com/stevenremot/svn-multi-commit.el

;;; License:

;; The MIT License (MIT)

;; Copyright (c) 2014 Steven Rémot

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

;;; Commentary:
;; Usage :
;;
;; In each branch's vc-mode, mark the files you want to commit and run
;;
;;   M-x svn-multi-commit-add
;;
;; When you have added all your file to the commit list, run
;;
;;   M-x svn-multi-commit-do
;;
;; You will be asked to confirme you want to commit *these* files, And
;; then you can enter your commit message.
;;
;; This code is currently very simple, feel free to suggest
;; enhancements.

(require 'vc-dir)

;;; Code:

(defcustom svn-multi-commit-template ""
  "Base value in commit message.")

(defvar svn-multi-commit--files '()
  "Files to commit.")

(defconst svn-multi-commit--log-buffer-name "*svn-multi-commit-log*")

(defconst svn-multi-commit--summary-buffer-name " *svn-multi-commit-files*")

;;;###autoload
(defun svn-multi-commit-add ()
  "Add the selected files to commit in mode `vc-mode'."
  (interactive)
  (dolist (f (vc-dir-marked-files))
    (when (member (vc-call-backend 'SVN 'state f) '(edited added removed))
      (message "Added %s to commit." f)
      (add-to-list 'svn-multi-commit--files f))))

(defun svn-multi-commit-reset ()
  "Empty the commit files list."
  (interactive)
  (message "Reset commit list.")
  (setq svn-multi-commit--files '()))

;;;###autoload
(defun svn-multi-commit-do ()
  "Commit the files that have been registered with `svn-multi-commit-add'.

You will be asked to confirm you want to commit *these* files.
Then you can enter a commit message in the mini buffer."
  (interactive)
  (let ((log-buffer (svn-multi-commit--create-log-buffer))
        (summary-buffer (svn-multi-commit--create-summary-buffer)))
    (pop-to-buffer log-buffer)
    (display-buffer summary-buffer 'display-buffer-other-window)))

(defun svn-multi-commit--create-log-buffer ()
  "Create the buffer in which log message will be written."
  (let ((buffer (get-buffer-create svn-multi-commit--log-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert svn-multi-commit-template)
      (svn-multi-commit-log-mode))
    buffer))

(defun svn-multi-commit--create-summary-buffer ()
  "Create the buffer that displays files to commit."
  (let ((buffer (get-buffer-create svn-multi-commit--summary-buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Files to commit :\n\n")
      (dolist (file svn-multi-commit--files)
        (insert (format "- %s\n" file))))
    buffer))

(defun svn-multi-commit-log-mode-send ()
  "Commit the files registered with `svn-multi-commit-add'.

Use the buffer content as log message."
  (interactive)
  (let* ((message (buffer-substring-no-properties (point-min) (point-max)))
         (args (append (list "ci"  "-m" message) svn-multi-commit--files)))
    (apply 'start-process "svn-multi-ci" "*ubeeqo-svn-ci*" "svn" args)
    (svn-multi-commit-reset)
    (svn-multi-commit-log-mode-close)))

(defun svn-multi-commit-log-mode-close ()
  "Close log windows."
  (interactive)
  (kill-buffer svn-multi-commit--log-buffer-name)
  (kill-buffer svn-multi-commit--summary-buffer-name))

(define-derived-mode svn-multi-commit-log-mode fundamental-mode "SVN MC Log"
  "Major mode for writing log message for svn-multi-commit.

\\{svn-multi-commit-log-mode-map}")

(define-key svn-multi-commit-log-mode-map (kbd "C-c C-c") #'svn-multi-commit-log-mode-send)
(define-key svn-multi-commit-log-mode-map (kbd "C-c C-k") #'svn-multi-commit-log-mode-close)

(provide 'svn-multi-commit)

;;; svn-multi-commit.el ends here
