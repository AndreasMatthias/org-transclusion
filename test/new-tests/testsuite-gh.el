(package-initialize t)
(use-package org
  :load-path "~/.config/emacs/dev-repo/org-mode/lisp")

(org-id-update-id-locations (directory-files-recursively "./files" ".org$"))

(load-file "testsuite.el")
