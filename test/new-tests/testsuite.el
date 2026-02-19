;;; ...  -*- lexical-binding: t -*-

;;
;; Check org-version
;;
(require 'org)
(when (version< org-version "9.8-pre")
  (display-warning 'testsuite
		   (format "
############################################################
# Org version `%s' is too old.
# Some tests will fail. In particluar there will be issues
# with blank lines in org files.
############################################################"
                           (org-version))))

;;
;; Load org-transclusion from current repository
;;
(cl-labels ((nth-parent-directory (nth path)
	      (if (<= nth 0)
		  (file-name-directory path)
		(nth-parent-directory
		 (1- nth)
		 (file-name-parent-directory (file-name-directory path))))))
  (add-to-list 'load-path
	       (nth-parent-directory 2 (or load-file-name buffer-file-name))))
(require 'org-transclusion)


;;
;; Helper functions
;;
(defun org-tc--create-detached-file (detached-file)
  "Save current buffer with all transclusions detached.
Used for writing new tests."
  (interactive (list (read-file-name "Save detached file: " "./" nil nil
				     (concat (file-name-sans-extension (buffer-name))
					     "-detached.org"))))
  (save-excursion
    (org-tc--apply-customizations
     (org-transclusion-remove-all)
     (org-transclusion-add-all))
    (write-region (point-min) (point-max) detached-file)))

(defun org-tc--update-ids-of-test-files ()
  "Scan test files for IDs and update the Org ID database."
  (let* ((dir (expand-file-name "./files"))
         (files (directory-files-recursively dir "\.org$")))
    (org-id-update-id-locations files)
    ))

(defmacro org-tc--apply-customizations (&rest body)
  "Make sure we all use the same customizations."
  `(let ((org-edit-src-content-indentation 2))
     ,@body))

(defun org-tc--unvisited-files (files)
  "Return unvisited files of FILES."
  (seq-filter (lambda (file)
                (if (get-file-buffer file)
                    nil
                  file))
              files))

(defun org-tc--check-test-file (filename-test &optional cleanup-files)
  "Compare FILENAME-TEST with its associated detached file.
CLEANUP-FILES is a list of file names (strings), that are opened during
the transclusion process. This function takes care to close all these files,
unless they were already open before running this function."
  (let* ((cleanup-files (org-tc--unvisited-files cleanup-files))
         (filename-expected (org-tc--get-detached-filename filename-test))
         (buf-test (org-tc--create-test-buffer filename-test))
	 (buf-expected (org-tc--create-test-buffer filename-expected))
         (res (org-tc--create-diff-buffer buf-test buf-expected))
         (buf-diff (plist-get res :buffer))
         (buffer-equal (plist-get res :equal)))
    (unwind-protect
        (ert-info ((org-tc--pretty-print-diff-buffer buf-diff)
                   :prefix "   ")
          (should buffer-equal))
      (kill-buffer buf-test)
      (kill-buffer buf-expected)
      (if buffer-equal
          (kill-buffer buf-diff))
      (dolist (file cleanup-files)
        (when-let ((buf (get-file-buffer file)))
          (kill-buffer buf))))))


(defun org-tc--get-detached-filename (filename)
  "Return the detached file name corresponding to file FILENAME."
  (concat (file-name-sans-extension filename)
          "-detached."
          (file-name-extension filename)))

(defun org-tc--create-test-buffer (filename)
  "Create a new buffer for FILENAME and transclude everything.
Return the buffer."
  (let ((buf (generate-new-buffer filename)))
    (with-current-buffer buf
      (insert-file-contents filename)
      (org-tc--apply-customizations
       (org-mode)
       (org-transclusion-add-all)
       (set-buffer-modified-p nil)))
    buf))

(defun org-tc--create-diff-buffer (buf-test buf-expected)
  "Create a diff for BUF-TEST and BUF-EXPECTED.
Return a plist with the following properties:
  :buffer   The buffer of the diff.
  :equal    Non-nil if buffers are equal."
  (let ((inhibit-read-only t)
        (buffers-equal (org-tc--buffers-are-equal-p buf-test buf-expected)))
    (diff-buffers buf-test buf-expected "-u" t)
    (with-current-buffer "*Diff*"
      (rename-buffer (format "*Diff %s*" buf-test) t)
      (goto-char (point-min))
      (delete-line)
      (diff-mode)
      (list :buffer (current-buffer) :equal buffers-equal))))

(defun org-tc--buffers-are-equal-p (buf-1 buf-2)
  "Compare buffers BUF-1 and BUF-2."
  (equal (compare-buffer-substrings buf-1 nil nil
                                    buf-2 nil nil)
         0))

(defun org-tc--goto-line-column (line col)
  "Move point to LINE and COL."
  (goto-char (point-min))
  (forward-line (1- line))
  (if (eq col 'end)
      (end-of-line)
    (move-to-column col)))

(defmacro org-tc--live-action (line column &optional action)
  "Goto LINE and COLUMN and run ACTION in live-editing."
  `(progn
     (org-tc--goto-line-column ,line ,column)
     (org-transclusion-live-sync-start)
     ,action
     (org-transclusion-live-sync-exit)
     (set-buffer-modified-p nil)))

(defun org-tc--live-insert-string (str line column)
  "Insert string STR at LINE and COLUMN in live-editing."
  (org-tc--live-action line column
                       (mapc (lambda (c)
	                       (self-insert-command 1 c))
	                     str)))

(defun org-tc--run-action-in-buffer (buf action)
  (save-window-excursion
    (pop-to-buffer buf)
    (org-mode)
    (org-transclusion-add-all)
    (funcall action)
    ))

(defun org-tc--check-test-file-live-edit (filename-test
                                          filename-test-detached
                                          filename-src
                                          filename-src-modified
				          action
                                          &optional cleanup-files)
  "Run test file FILENAME-TEST with live-editing ACTION.
Apply ACTION to the buffer visiting FILENAME-TEST.
CLEANUP-FILES is a list of files names (strings). Each file will have its
associated buffer killed via `kill-buffer' before this function returns,
even if the test fails."
  (let* ((exists (get-file-buffer filename-src))
         (tc-buf-1 (org-tc--create-test-buffer filename-test))
         (tc-buf-2 (org-tc--create-test-buffer filename-test-detached))

         (src-buf-1-capture (org-tc--get-buffer-snapshot filename-src exists))
         (src-buf-1 (plist-get src-buf-1-capture :buffer))
         (src-buf-2 (org-tc--create-test-buffer filename-src-modified))

         ;; The buffers opened earlier shall not be killed if `action'
         ;; breaks the test here.
         (_ (org-tc--run-action-in-buffer tc-buf-1 action))
         
         (res (org-tc--create-diff-buffer tc-buf-1 tc-buf-2))
         (tc-buf-diff (plist-get res :buffer))
         (tc-buf-equal (plist-get res :equal))

         (res (org-tc--create-diff-buffer src-buf-1 src-buf-2))
         (src-buf-diff (plist-get res :buffer))
         (src-buf-equal (plist-get res :equal)))
    (unwind-protect
        (ert-info ((concat
                    (when (not tc-buf-equal)
                      (org-tc--pretty-print-diff-buffer tc-buf-diff))
        	    (when (not src-buf-equal)
                      (org-tc--pretty-print-diff-buffer src-buf-diff)))
                   :prefix "   ")
          (should (and tc-buf-equal src-buf-equal)))

      (kill-buffer tc-buf-1)
      (kill-buffer tc-buf-2)
      (when tc-buf-equal
        (kill-buffer tc-buf-diff))
      
      (org-tc--kill-or-reset-buffer src-buf-1 src-buf-1-capture)
      (kill-buffer src-buf-2)
      (when src-buf-equal
        (kill-buffer src-buf-diff))

      (dolist (file cleanup-files)
        (when-let ((buf (get-file-buffer file)))
          (kill-buffer buf))))))

(defun org-tc--kill-or-reset-buffer (buffer kill-or-snapshot)
  "Kill or reset BUFFER dependig on KILL-OR-SNAPSHOT.
KILL-OR-SNAPSHOT is either a boolean or a plist that function
`org-tc--get-buffer-snapshot' returned.
Kill the buffer if either KILL-OR-SNAPSHOT is non-nil or the
property `:existed' of the plist is nil.
Reset the buffer if `:existed' is non-nil."
  (let* ((capture (when (consp kill-or-snapshot) kill-or-snapshot))
         (reset (plist-get capture :existed))
         (do-kill (if capture
                      (not (plist-get capture :existed))
                    kill-or-snapshot)))
    (when (and capture reset)
      (org-tc--reset-buffer capture))
    (when do-kill
      (with-current-buffer buffer
        (set-buffer-modified-p nil)
        (kill-buffer)))))

(defun org-tc--get-buffer-snapshot (filename exists)
  "Capture the state of the buffer visiting FILENAME.
Return a plist with the following properties:
  :buffer   The buffer object.
  :existed  Non-nil if the buffer existed before this call.
  :data     The original content of the buffer.
  :point    The original point of the buffer."
  (let* ((buf (get-file-buffer filename))
         (data (when exists
                 (with-current-buffer buf
                   (buffer-string))))
         (point (when exists
                  (with-current-buffer buf
                    (point)))))
    (list :buffer buf
          :existed exists
          :data data
          :point point)))

(defun org-tc--reset-buffer (snapshot)
  "Reset a buffer as specified in the plist SNAPSHOT.
SNAPSHOT is the plist return by `org-tc--get-buffer-snapshot'."
  (let* ((buffer (plist-get snapshot :buffer))
         (data (plist-get snapshot :data))
         (point (plist-get snapshot :point)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert data)
        (goto-char point)
        (set-buffer-modified-p nil)))))

(defun org-tc--diff-buffer-name (filename)
  "Return the name for a diff buffer diffing file FILENAME."
  (format "*Diff %s*" (file-name-nondirectory filename)))

(defun org-tc--pretty-print-diff-buffer (buf)
  "Return pretty-printed buffer-string of BUF, used as an ERT error message."
  (cl-flet* ((decorate (str)
               (propertize str 'face 'font-lock-doc-markup-face))
             (rule ()
               (concat (decorate (make-string 50 ?―)) "\n"))
             (button (buf)
               (concat (decorate "See also ")
                       (buttonize (buffer-name buf)
                                  (lambda (ov)
                                    (switch-to-buffer buf)))
                       (decorate ".\n"))))
    (concat (rule)
            (with-current-buffer buf (buffer-string))
            (rule)
            (button buf)
            "\n")))

;;
;; Display metadata in ERT buffer
;;
(defun org-tc--display-metadata (&rest _)
  "Display metadata in ERT buffer."
  (let* ((sep (concat (make-string 30 ?–) "\n"))
         (header "Testing environment:\n")
         (emacs (format "  • Emacs version: %s\n" emacs-version))
         (org (format "  • Org version: %s\n" org-version))
         (info (concat sep header emacs org sep))
         (info-pp (propertize info 'face 'font-lock-doc-markup-face)))
    (if (get-buffer "*ert*")
      (with-current-buffer "*ert*"
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char (point-max))
            (insert info-pp))))
      (princ (format "%s" info)))))

(advice-add 'ert-run-tests :after #'org-tc--display-metadata)



;;
;; Tests
;;
(ert-deftest org-tc--test-blank-lines-1 ()
  "Blank lines at end of files."
  (org-tc--check-test-file "test-blank-lines-1.org"
                           '("./files/blank-lines-1.org"
                             "./files/blank-lines-1.el")))

(ert-deftest org-tc--test-blank-lines-2 ()
  "Blank lines at end of Org sections."
  (org-tc--check-test-file "test-blank-lines-2.org"
                           '("./files/blank-lines-2.org")))

(ert-deftest org-tc--test-exclude-elements ()
  "Property `:exclude-elements'."
  (org-tc--check-test-file "test-exclude-elements.org"
                           '("./files/elements.org")))

(ert-deftest org-tc--test-expand-links ()
  "Property `:expand-links'."
  (let* ((buf-test (org-tc--create-test-buffer "test-expand-links.org"))
         (buf-expected (let* ((file "test-expand-links-detached.org")
                              (buf (org-tc--create-test-buffer file))
                              (dirname (file-name-directory (expand-file-name file))))
                         (with-current-buffer buf
                           (goto-char (point-min))
                           (while (search-forward "<CURRENT-DIR>" nil t)
                             (replace-match dirname t)))
                         buf))
         (res (org-tc--create-diff-buffer buf-test buf-expected))
         (buf-diff (plist-get res :buffer))
         (buf-equal (plist-get res :equal)))
    (unwind-protect
        (ert-info ((org-tc--pretty-print-diff-buffer buf-diff)
                   :prefix "   ")
          (should buf-equal))
      (kill-buffer buf-test)
      (kill-buffer buf-expected)
      (kill-buffer buf-diff)
      (kill-buffer "expand-links.org")
      )))

(defun org-tc--replace-dirname (buf dirname)
  (with-current-buffer buf
    (while (search-forward "<CURRENT-DIR>" nil t)
      (replace-match dirname t))
    (set-buffer-modified-p nil)))

(ert-deftest org-tc--test-level ()
  "Property `:level'."
  (org-tc--check-test-file "test-level.org"
                           '("./files/links-1.org")))

(ert-deftest org-tc--test-links ()
  "Org links: UUID, custom-id, dedicated, internal, ..."
  (org-tc--check-test-file "test-links.org"
                           '("./files/links-1.org")))

(ert-deftest org-tc--test-live-edit-1a ()
  "Live edit (org file): Insert a strings."
  (org-tc--check-test-file-live-edit "test-live-edit-1.org"
                                     "test-live-edit-1a-detached.org"
                                     "./files/live-edit-1.org"
                                     "./files/live-edit-1a-modified.org"
                                     (lambda ()
                                       (org-tc--live-insert-string "modified " 9 7)
                                       (org-tc--live-insert-string " end" 11 'end))))

(ert-deftest org-tc--test-live-edit-1b ()
  "Live edit (org file): Insert a new line."
  (org-tc--check-test-file-live-edit "test-live-edit-1.org"
                                     "test-live-edit-1b-detached.org"
                                     "./files/live-edit-1.org"
                                     "./files/live-edit-1b-modified.org"
                                     (lambda ()
                                       (org-tc--live-insert-string "\na new line" 9 'end))))

(ert-deftest org-tc--test-live-edit-1c ()
  "Live edit (org file): Insert a new paragraph."
  (org-tc--check-test-file-live-edit "test-live-edit-1.org"
                                     "test-live-edit-1c-detached.org"
                                     "./files/live-edit-1.org"
                                     "./files/live-edit-1c-modified.org"
                                     (lambda ()
                                       (org-tc--live-insert-string "\n\na new paragraph" 9 'end))))

(ert-deftest org-tc--test-live-edit-2a ()
  "Live edit (org file): Do nothing.
Actually we start a live-edit, but quit it instantly."
  (org-tc--check-test-file-live-edit "test-live-edit-2.org"
                                     "test-live-edit-2a-detached.org"
                                     "./files/live-edit-2.org"
                                     "./files/live-edit-2a-modified.org"
                                     (lambda ()
                                       (org-tc--live-action 7 0)
                                       (org-tc--live-action 9 'end))))

(ert-deftest org-tc--test-live-edit-3a ()
  "Live edit (source code)"
  :expected-result :failed
  (org-tc--check-test-file-live-edit "test-live-edit-3.org"
                                     "test-live-edit-3a-detached.org"
                                     "./files/live-edit-3.el"
                                     "./files/live-edit-3a-modified.el"
                                     (lambda ()
                                       (org-tc--live-insert-string "\n  (message \"new\")" 9 'end))))

(ert-deftest org-tc--test-noweb-chunk ()
  "Property `:noweb-chunk'."
  (org-tc--check-test-file "test-noweb-chunk.org"
                           '("./files/noweb-chunk-1.nw"
                             "./files/noweb-chunk-2.nw"
                             "./files/noweb-chunk-3.nw"
                             "./files/noweb-chunk-4.nw")))

(ert-deftest org-tc--test-only-contents ()
  "Property `:only-contents'."
  (org-tc--check-test-file "test-only-contents.org"
                           '("./files/links-1.org")))

(ert-deftest org-tc--test-src-blocks ()
  "Property `:src'."
  :expected-result :failed
  (org-tc--check-test-file "test-src-blocks.org"
                           '("./files/code-1.el"
                             "./files/code-2.el")))

(ert-deftest org-tc--test-thing-at-point ()
  "Property `:thing-at-point'."
  (org-tc--check-test-file "test-thing-at-point.org"
                           '("./files/thing-at-point.txt"
                             "./files/thing-at-point.org"
                             "./files/thing-at-point.el"
                             )
  ))
