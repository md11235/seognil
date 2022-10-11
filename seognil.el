;;; seognil.el --- query words in massaged dictionaries in lingoes format -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2012 Madsen Zhang

;; Author: md11235@gmail.com
;; Created: 2012-12-02

;; NOTE:  THIS IS A BETA VERSION OF SEOGNIL.  USE AT YOUR OWN RISK.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is an emacs lisp script to query words in dictionaries in lingoes format

;; How to massage one dictionary in the lingoes format to be usable by
;; this program. suppose that one dictionary is named "ADict".

;; 1. download a java program from the url below:
;; http://dict4cn.googlecode.com/svn/trunk/importer/src/LingoesLd2Reader.java
;; 2. compile it
;; 3. use the compiled java program to dump contents of ADict.ld2
;; 4. rename "ADict.ld2.output" to "ADict.ld2.dict"
;; 5. it turns out that "ADict.ld2.idx" generated from step 3 does not have
;;    all the words defined in the "ADict.ld2.dict" file. so regenerate the
;;    idx file using one line of awk. for example:
;;    awk -F "=" 'BEGIN {x=0}; {print $1 ", " x; x++}' ADict.ld2.dict > ADict.ld2.idx
;; 6. sort the content of the new .idx file inside emacs using
;;    "seognil-sort-fields" (which is defined in this program file)
;;    with the elisp variable "sort-fold-case" being nil.
;; 7. move "ADict.ld2.idx" and "ADict.ld2.dict" into a directory named
;;    "ADict".
;;    7.1 [OPTIONAL] use gzip to compress "ADict.ld2.dict" and set the variable
;;        `seognil-use-gzipped-dictionaries' to t, in order to save harddisk space.
;; 8. repeat steps 1~8 above for another dictionary.

;; finaly, put all these dictionary directories, say "ADict", "BDict", "CDict"
;; into another directory, for example "d:/src/lingoes/dict".

;; Put this file into your load-path and the following into your

;; ~/.emacs:
;; (require 'seognil)
;; (setq seognil-dictionary-path "d:/src/lingoes/dict")
;; (setq seognil-dictionaries '("ADict" "BDict" "CDict"))
;; (global-set-key (kbd "C-c d") 'seognil-search)

;;; Code:

(require 'w3m)
(require 'thingatpt)
(require 'parse-time)
(require 'emacsql)
(require 'emacsql-sqlite)

(defconst *WORD-INDEX-SEPARATOR* ", "
  "the string between the word and its index position in the dict file")

(defconst *DICT-FILENAME-EXTENSION* ".sqlite"
  "the postfix for dictionary file(where words and their definitions are stored")

(defconst *INDEX-FILENAME-EXTENSION* ".ld2.idx"
  "the postfix for index file(where words and their definitions are stored")

(defvar seognil-dictionary-path nil
  "the fullpath of the parent directory containing different folders of dictionaries")

(defvar seognil-dictionaries nil
  "the list of dictionaries enabled")

(defvar seognil-use-gzipped-dictionaries nil
  "whether dictionaries are compressed using gzip.")

(defvar seognil-buffer-name "*seognil*"
  "The name of the buffer of seognil.")

;; copied from http://www.emacswiki.org/emacs/ElispCookbook#toc6

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun seognil-query-word-definition-in-dict (dictionary-name word)
  (emacsql-with-connection
      (dict-db (emacsql-sqlite (concat seognil-dictionary-path "/" dictionary-name "/" dictionary-name *DICT-FILENAME-EXTENSION*)))
    (caar (emacsql dict-db
                   [:select [definition]
                            :from word_definitions
                            :where (= word $s1)]
                   word))))

(defun seognil-search-phrase (word)
    (with-current-buffer (get-buffer-create seognil-buffer-name)
          (setq buffer-read-only nil)
          (erase-buffer)

          (cl-loop for dict in seognil-dictionaries
                do (progn
                     (insert (concat "[dict: " dict "]<br/> <br/>"))
                     (let ((result (seognil-query-word-definition-in-dict dict word)))
                       (if result
                           (insert result)
                         (insert "No definitions found.")
                         ))
                     (insert "<br/> <br/>")))
          
          (w3m-buffer)
          (switch-to-buffer seognil-buffer-name)
    (set (make-local-variable 'w3m-goto-article-function) #'seognil-goto-article)
    (w3m-minor-mode t)
    (setq buffer-read-only t)))

;;;; dict-url is like "dict://key.[$DictID]/hit%20and%20miss"
(defun seognil-goto-article (dict-url)
  (let ((phrase (cadr (split-string (w3m-url-decode-string dict-url) "]/"))))
    (seognil-search-phrase phrase)
    t))

(defun seognil-search ()
  "read the WORD from mini buffer and query it in DICTIONARY-NAME"
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (setq word (chomp (read-string (if word
                                       (format "Query Word(default %s):" word)
                                     (format "Query Word:"))
                                   nil
                                   nil
                                   word)))
    (seognil-search-phrase word)))

;;;; utils
(defun seognil-parse-word-index-pair (line-content)
  (let ((current-line-content line-content)
        word
        index)
    (setq word (car (split-string line-content
                                  ", [0-9]+")))
    (setq index (substring current-line-content (+ 2 (length word)) -1))
    (cons word index)))

(defun seognil-sort-fields (field beg end)
  "Sort lines in region lexicographically by the ARGth field of each line.
Fields are separated by whitespace and numbered from 1 up.
With a negative arg, sorts by the ARGth field counted from the right.
Called from a program, there are three arguments:
FIELD, BEG and END.  BEG and END specify region to sort.
The variable `sort-fold-case' determines whether alphabetic case affects
the sort order.
sort specifically for seognil.."
  (interactive "p\nr")
  (let ;; To make `end-of-line' and etc. to ignore fields.
      ((inhibit-field-text-motion t))
    (sort-fields-1 field beg end
		   (function (lambda ()
			       (sort-skip-fields field)
			       nil))
		   ;; (function (lambda () (skip-chars-forward "^,\t\n")))
           (function (lambda ()
                       (looking-at ".*\\(, [0-9]+\\)")
                       (goto-char (match-beginning 1)))))))

(provide 'seognil)
