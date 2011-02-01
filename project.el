;; Project shortcuts:
;; grep (f4), svn up (f5), svn diff (f6), svn status (f7), svn commit (f8)

(setq project-root nil)

(defun get-project-root ()
  (if (not project-root)
      (call-interactively
       (lambda (root)
	 (interactive "DEnter project root dir: ")
	 (setq project-root root))))
    project-root)


(defun grep-project (s)
  (interactive "sSearch project for: ")
  (grep-find (concat "find " (get-project-root) " -type f -not -name \"*.svn-base\" -and -not -name \"*.tmp\" -and -not -name \"*.log\" -and -not -name \"*.xml\" -and -not -name \"*.svn\"  -and -not -name \"*.pyc\"  -and -not -name \"entries\" -and -not -name \"*.psql\" -print0 | xargs -0 -e grep -n -s -w \
-F \"" s "\"")))
(global-set-key (quote [f4]) (quote grep-project))


(defun svn-update-project ()
  (interactive)
  (shell-command (concat "svn up " (get-project-root)))
  (with-current-buffer "*Shell Command Output*"
    (normal-mode)))
(global-set-key (quote [f5]) (quote svn-update-project))


(defun svn-diff-project ()
  (interactive)
  (shell-command (concat "svn diff " (get-project-root)))
  (with-current-buffer "*Shell Command Output*"
    (diff-mode)))
(global-set-key (quote [f6]) (quote svn-diff-project))


(defun svn-status-project ()
  (interactive)
  (shell-command (concat "svn status " (get-project-root)))
  (with-current-buffer "*Shell Command Output*"
    (normal-mode)))
(global-set-key (quote [f7]) (quote svn-status-project))


(defun svn-commit-project (message)
  (interactive "sEnter commit message: ")
  (shell-command (concat "svn commit " (get-project-root) " -m '" message "'"))
  (with-current-buffer "*Shell Command Output*"
    (normal-mode)))
(global-set-key (quote [f8]) (quote svn-commit-project))


(provide 'project)