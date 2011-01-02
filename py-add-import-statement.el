(defun add-import-statement ()
  "Inserts import statement for word at cursor at the beginning of the file.
   Searches for similar import statements in all open buffers.
   If none are found, prompts user"
  (interactive)
  (let ((word (thing-at-point 'word))
	(current (current-buffer))
	import-statement)
    (setq import-statement (find-import-string word))
    (if (not import-statement)
	(progn
	  (let ((buffer (next-user-buffer)))
	    (while (and (not (equal buffer current)) (not import-statement))
	      (setq import-statement (find-import-string word))
	      (if import-statement
		  (progn
		    (set-buffer current)
		    (insert-import-statement import-statement))
		(setq buffer (next-user-buffer)))))
	  (if (not import-statement)
	      (insert-import-statement
	       (read-string "Import statement: " (concat "import " word))))))
    (switch-to-buffer current)))


(defun insert-import-statement (import-statement)
  "Insert import-statement before the first import in the current buffer"
  ;; TODO - add name to "from" list if possible
  (save-excursion  
    (goto-char (point-min))
    (search-forward "import" nil t)
    (beginning-of-line)
    (insert import-statement) (newline)))

(defconst py-symbols "a-zA-Z_0-9")

(defun find-import-string (word)
  "Find string that imports the desired word"
  (save-excursion
    (goto-char (point-min))
    (let ((import-pos
	   (re-search-forward (concat "import[" py-symbols " ,]* " word
				      "[" py-symbols " ,]*$")
			      nil t)))
      (if import-pos
	  ;; TODO - import of modules (like import os.path or from foo import bar.baz)
	  (let ((import-string (thing-at-point 'line)))
	    (if (string-match (concat "^\\(from [" py-symbols "]+\\) import ")
			      import-string)
		(concat (match-string 1 import-string) " import " word)
	      (concat "import " word)))))))


(defun next-user-buffer ()
  "Iterate over all user buffers. User buffers are those not starting with *."
  (next-buffer)
  (while (string-match "^*" (buffer-name))
      (next-buffer))
  (current-buffer))


(provide 'py-add-import-statement)