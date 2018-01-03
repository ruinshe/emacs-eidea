;;; eidea.el --- A plugin to manage / build contest problems.

;;; Copyright (C) 2017 ruinshe

;; Author: Ruins He <lyhypacm@gmail.com>
;; URL: https://github.com/ruinshe/emacs-eidea
;; Version: 0.3
;; Package-Requires: (org, multi-term)

;;; Commentary:

;; To use this file, put something like the following in your ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/eidea/")
;; (require 'eidea)
;;
;; Type M-x eidea/show to show the eidea dashboard.
;;
;; To set options for EIdea, type M-x customize, then select eidea group.

;;; Code:

(require 'org)
(require 'multi-term)

(defgroup eidea nil
  "Emacs & Rime integration extension group."
  :link "https://github.com/ruinshe/emacs-eidea"
  :group 'tools)
(defcustom eidea/workdir nil
  "Working directory of rime, which includes a PROJECT file."
  :type 'string
  :group 'eidea)

(defconst eidea/buffer "*EIdea*")
(defconst eidea/project-prediction "PROJECT")
(defconst eidea/problem-prediction "PROBLEM")
(defconst eidea/solution-prediction "SOLUTION")
(defconst eidea/testset-prediction "TESTSET")

(defvar eidea-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map "g" 'eidea/show)
    (define-key map "q" 'eidea/close-then-delete)
    map))

(define-derived-mode eidea-mode
  org-mode "EIdea"
  "Major mode for eidea.")

(defun eidea/close-then-delete ()
  "Close the WINDOW then delete it."
  (interactive)
  (switch-to-buffer eidea/buffer)
  (kill-buffer eidea/buffer))

(defun eidea/render-buffer ()
  "Render the EIdea buffer."
  (interactive)
  (switch-to-buffer eidea/buffer)
  (erase-buffer)
  (setq org-confirm-elisp-link-function nil)
  (eidea-mode)

  (insert "[[elisp:(eidea/clean-workspace)][Clean workspace]]\n\n")
  (insert "Use =g= command to refresh this buffer.\n\n")
  (insert "|--\n")
  (insert "|#|Problem|Solutions and tests|Invocations|\n")
  (insert "|--\n")
  (let ((problem-files (directory-files eidea/workdir t)) (counter 0))
    (dolist (problem-folder problem-files)
      (if (file-exists-p
           (expand-file-name eidea/problem-prediction problem-folder))
          (let ((problem-folder-name
                 (file-name-nondirectory problem-folder)))
            (progn
              (setq counter (1+ counter))
              (let ((subdirs (directory-files problem-folder t)) (solutions 0) (tests 0))
                ;; calculation
                (dolist (subdir subdirs)
                  (if (file-exists-p
                       (expand-file-name eidea/solution-prediction subdir))
                      (setq solutions (1+ solutions)))
                  (if (file-exists-p
                       (expand-file-name eidea/testset-prediction subdir))
                      (setq tests (1+ tests)))
                  )

                ;; problem header
                (insert "|" (number-to-string counter)
                        "|" (eidea/extract-problem-name problem-folder)
                        "||[[elisp:(find-file \""
                        (expand-file-name eidea/problem-prediction problem-folder)
                        "\")][Config]] / [[elisp:(eidea/problem-build \""
                        problem-folder-name "\")][Build]] / "
                        "[[elisp:(eidea/problem-test \""
                        problem-folder-name "\")][Test]]"
                        "|\n")
                (insert "|--\n")
                
                ;; solutions
                (insert "|||" (number-to-string solutions) " solution(s)\n")
                (dolist (subdir subdirs)
                  (if (file-exists-p
                       (expand-file-name eidea/solution-prediction subdir))
                      (insert "|||" (file-name-nondirectory subdir) "|"
                              "[[elisp:(find-file \""
                              (expand-file-name eidea/solution-prediction subdir)
                              "\")][Config]] / [[elisp:(find-file \"" subdir "\")][Folder]]|"
                              "\n")))
                (insert "|||[[elisp:(eidea/add-solution \""
                        problem-folder "\")][Add solution]]||\n")
                (insert "|--\n")

                ;; tests
                (insert "|||" (number-to-string tests) " test(s)\n")
                (dolist (subdir subdirs)
                  (if (file-exists-p
                       (expand-file-name eidea/testset-prediction subdir))
                      (insert "|||" (file-name-nondirectory subdir) "|"
                              "[[elisp:(find-file \""
                              (expand-file-name eidea/testset-prediction subdir)
                              "\")][Config]] / [[elisp:(find-file \"" subdir "\")][Folder]]|"
                              "\n")))
                (insert "|||[[elisp:(eidea/add-testset \""
                        problem-folder "\")][Add testset]]"
                        "||\n")
                (insert "|--\n")))))))
  (org-table-align)
  (read-only-mode))

(defun eidea/create-new-buffer ()
  "Create a new buffer to list all problems."
  (interactive)
  (if (get-buffer eidea/buffer) (eidea/close-then-delete))
  (generate-new-buffer eidea/buffer)
  (eidea/render-buffer))

(defun eidea/show ()
  "Show the eidea pane."
  (interactive)
  (if (eidea/check-workdir)
      (eidea/create-new-buffer)
    (error "The working directory is not valid")))

(defun eidea/check-workdir ()
  "Check the workdir exists and valid for rime project."
  (interactive)
  (let ((file (expand-file-name eidea/project-prediction eidea/workdir)))
    (file-exists-p file)))

(defun eidea/run-rime-command (command)
  "Run rime COMMAND."
   (multi-term-prev)
   (term-send-raw-string
    (concat "cd " eidea/workdir " && rime " command "\n")))

(defun eidea/clean-workspace ()
  "Clean the rime workspace."
  (interactive)
  (eidea/run-rime-command "clean"))

(defun eidea/problem-build (problem)
  "Build specific PROBLEM."
  (interactive)
  (eidea/run-rime-command (concat "build " problem)))

(defun eidea/problem-test (problem)
  "Build specific PROBLEM."
  (interactive)
  (eidea/run-rime-command (concat "test " problem)))

(defun eidea/add-solution (problem-folder)
  "Add solution in PROBLEM-FOLDER."
  (let ((solution (read-string "Solution name: ")))
    (let ((subdir (expand-file-name solution problem-folder)))
      (if (file-exists-p subdir)
          (error "The folder exist, please check solutions and testsets")
        (eidea/run-rime-command
         (concat "add "
                 (file-name-nondirectory problem-folder) " solution "
                 (file-name-nondirectory subdir)))))))

(defun eidea/add-testset (problem-folder)
  "Add solution in PROBLEM-FOLDER."
  (let ((solution (read-string "Testset name: ")))
    (let ((subdir (expand-file-name solution problem-folder)))
      (if (file-exists-p subdir)
          (error "The folder exist, please check solutions and testsets")
        (eidea/run-rime-command
         (concat "add "
                 (file-name-nondirectory problem-folder) " testset "
                 (file-name-nondirectory subdir)))))))

(defun eidea/extract-problem-name (problem-folder)
  "Extract the problem name of a problem located in PROBLEM-FOLDER."
  (setq config-file (expand-file-name eidea/problem-prediction problem-folder))
  (setq more-lines t)
  (find-file config-file)
  (goto-char 1)
  (search-forward-regexp "title[ ]*=[ ]*\"" nil t)
  (setq problem-name-start (point))
  (search-forward "\"")
  (setq problem-name-end (point))
  (setq result (buffer-substring-no-properties problem-name-start (1- problem-name-end)))
  (kill-buffer (current-buffer))
  result)

(provide 'eidea)
;;; eidea.el ends here
