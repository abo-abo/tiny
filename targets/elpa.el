;; Copyright (C) 2017  Free Software Foundation, Inc.

(setq package-user-dir
      (expand-file-name (format ".cask/%s/elpa" emacs-version)))
(package-initialize)
(add-to-list 'load-path default-directory)
