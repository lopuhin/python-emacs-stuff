Python Emacs Stuff
==================

Add import statement
--------------------
Inserts import statement for word at cursor at the beginning of the file. Searches for similar import statements in all open buffers. If none are found, prompts user.

TODO:
 * respect from __future__ import ... (should be the first import statement)

Project shortcuts (mostly SVN inegration)
-----------------------------------------
Simple shortcuts for common project operations:
 * grep project (f4)
 * svn up of project (f5)
 * svn diff of project (f6) - showing diff in diff mode
 * svn status of project (f7)
 * svn commit (f8) - just enter commit message and press <Enter>
