(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(dolist (package '(org multi-term))
  (unless (package-installed-p package)
    (package-install package)))

(require 'org)
(require 'multi-term)
