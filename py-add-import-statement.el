(defun add-import-statement ()
  "Inserts import statement for word at cursor at the beginning of the file.
   Searches for similar import statements in all open buffers.
   If none are found, prompts user"
  (interactive)
  (let ((word (thing-at-point 'word))
	(current (current-buffer))
	import-statement)
    (setq import-statement (find-import-statement word))
    (if (not import-statement)
	(progn
	  (let ((buffer (next-user-buffer)))
	    (while (and (not (equal buffer current)) (not import-statement))
	      (setq import-statement (find-import-statement word))
	      (if import-statement
		  (progn
		    (set-buffer current)
		    (insert-import-statement import-statement))
		(setq buffer (next-user-buffer)))))
	  (if (not import-statement)
	      (insert-import-statement
	       (read-string "Import statement: " (concat "import " word))))))
    (switch-to-buffer current)))


(defconst py-symbols "a-zA-Z_0-9\.")


(defun insert-import-statement (import-statement)
  "Add import to existing 'from module import ...' or
  insert import-statement before the first import in the current buffer"
  (let (inserted)
    (save-excursion
      (goto-char (point-min))
      (if (string-match
	   (concat "^\\(from [" py-symbols "]+ import\\) \\([" py-symbols "]+\\)$")
	   import-statement)
	  (let ((from-part (match-string 1 import-statement))
		(word (match-string 2 import-statement)))
	    ;; search for statement with the same "from" part
	    (if (search-forward from-part nil t)
		(progn
		  (while (string-match "\\\\$" (thing-at-point 'line))
		    (next-line))
		  (end-of-line)
		  (insert (concat ", " word))
		  (setq inserted t)))))
      (if (not inserted)
	  (progn ;; then insert before the first import statement in the file
	    (goto-char (point-min))
	    (if (not (search-forward "import" nil t))
		(progn ;; insert at the first not comment line
		  (while (string-match "^[ ]*#" (thing-at-point 'line))
		    (next-line))
		  (newline)))
	    (beginning-of-line)
	    (insert import-statement) (newline))))))


(defun find-import-statement (word)
  "Find string that imports the desired word"
  (save-excursion
    (goto-char (point-min))
    (let ((import-pos
	   (re-search-forward
	    (concat "import[" py-symbols " ,]* " word "[" py-symbols " ,]*$")
	    nil t)))
      (if import-pos
	  ;; TODO - import of modules (like import os.path or from foo import bar.baz)
	  (let ((import-statement (thing-at-point 'line)))
	    (if (string-match (concat "^\\(from [" py-symbols "]+\\) import ")
			      import-statement)
		(concat (match-string 1 import-statement) " import " word)
	      (concat "import " word)))))))


(defun next-user-buffer ()
  "Iterate over all user buffers. User buffers are those not starting with *."
  (next-buffer)
  (while (string-match "^*" (buffer-name))
      (next-buffer))
  (current-buffer))


(provide 'py-add-import-statement)