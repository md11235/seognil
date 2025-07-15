(require 'emacsql)

(defvar dict-db (emacsql-connect "CollinsCobuild2.sqlite"))

(emacsql-close dict-db)

(defun read-lines (filepath)
   "Return a list of lines of a file at filepath."
   (with-temp-buffer
     (insert-file-contents filepath)
     (split-string (buffer-string) "\n" t)))

(defun convert-old-dict-file (old-dict-filepath sqlite3-database-file)
  (emacsql-with-connection (dict-db (emacsql-sqlite sqlite3-database-file))
    (mapc (lambda (line)
            (let* ((parts (split-string line "="))
                   (word (car parts)))
              (emacsql dict-db [:insert :into word_definitions
                                        :values ([$s1 $s2])]
                       word line)))
          (read-lines old-dict-filepath))))

(convert-old-dict-file "Collins.ld2.dict" "Collins.sqlite")
(convert-old-dict-file "CollinsCobuild.ld2.dict" "CollinsCobuild.sqlite")