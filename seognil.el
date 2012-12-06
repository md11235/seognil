;;; seognil.el --- a program to query words in massaged dictioneries
;;; of lingoes format

;; Copyright 2012 Madsen Zhang
;;
;; Author: md11235@gmail.com

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
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; This is an emacs lisp script to query words in dictionaries of lingoes format

;; Put this file into your load-path and the following into your
;; ~/.emacs:
;; (require 'seognil)
;; (setq seognil-dictionary-path "d:/src/lingoes/dict")
;; (setq seognil-dictionaries '("CollinsCobuild" "Collins"))
;; (global-set-key (kbd "C-c d") 'seognil-search)


;;; Code:

(require 'w3m)

(provide 'seognil)

(defconst *WORD-INDEX-SEPARATOR* ", "
  "the string between the word and its index position in the dict file")

(defconst *DICT-FILENAME-EXTENSION* ".ld2.dict.gz"
  "the postfix for dictionary file(where words and their definitions are stored")

(defconst *INDEX-FILENAME-EXTENSION* ".ld2.idx"
  "the postfix for dictionary file(where words and their definitions are stored")

(defvar seognil-dictionary-path nil
  "the fullpath of the parent directory containing different folders of dictionaries")

(defvar seognil-dictionaries nil
  "the list of dictionaries enabled")

(defvar seognil-buffer-name "*seognil*"
  "The name of the buffer of seognil.")

(defun seognil-word-definition-position (dictionary-name word)
  ;; with-current-buffer "collins2.ld2.idx"
  (with-temp-buffer
    (insert-file-contents (concat seognil-dictionary-path
                                  "/"
                                  dictionary-name
                                  "/"
                                  dictionary-name *INDEX-FILENAME-EXTENSION*))
    (let ((end-result nil)
          (begin-line-number 1)
          (end-line-number (count-lines 1
                                        (progn (end-of-buffer)
                                               (point)))))
      (while (<= begin-line-number end-line-number)
        (let ((middle-line-number (/ (+ begin-line-number end-line-number) 2)))
          (goto-line middle-line-number)
          (beginning-of-line)
          (let ((current-word (progn
                                (looking-at "[^,]*,")
                                (buffer-substring-no-properties (match-beginning 0)
                                                                (- (match-end 0) 1)))))
            (message "word: %s, %d, %d, %d\n" current-word middle-line-number begin-line-number end-line-number)
            (cond
             ((string-equal word current-word)
              (setq begin-line-number (+ end-line-number 1)) ;; break out of the while
              (looking-at ",")
              (goto-char (match-beginning 0))
              (let* ((result (split-string (buffer-substring-no-properties (point)
                                                                           (progn
                                                                             (end-of-line)
                                                                             (point)))
                                           *WORD-INDEX-SEPARATOR*)))
                (message "got word: %s, line number: %s\n" (car result) (nth 1 result))
                (setq end-result result)))
             ((string-lessp word (downcase current-word))
              (setq end-line-number (- middle-line-number 1)))
             (t
              (setq begin-line-number (+ middle-line-number 1)))))))
      end-result)))

(defun seognil-extract-word-definition-in-dict (dictionary definition-line-number)
  (with-temp-buffer
    (insert-file-contents (concat seognil-dictionary-path
                                  "/"
                                  dictionary-name
                                  "/"
                                  dictionary-name *DICT-FILENAME-EXTENSION*))
    (goto-line (+ 1 (parse-integer definition-line-number)))
    (buffer-substring-no-properties (point)
                                    (progn
                                      (end-of-line)
                                      (point)))))

(defun seognil-query-word-definition-in-dict (dictionary-name word)
  (let ((word-index-cons (seognil-word-definition-position dictionary-name word)))
    (if (consp word-index-cons)
        (seognil-extract-word-definition-in-dict dictionary-name (nth 1 word-index-cons))
      nil)))

(defun seognil-search ()
  "read the WORD from mini buffer and query it in DICTIONARY-NAME"
  (interactive)
  (let (word
        ;; fixme: parse these two numbers on the fly
        (begin-line-number 1)
        (end-line-number 102385))
    (setq word (read-from-minibuffer "Word:"))

    (with-current-buffer (get-buffer-create seognil-buffer-name)
          (setq buffer-read-only nil)
          (erase-buffer)

          ;; collect the result from seognil-query-word-definition-in-dict
          ;; if all returned values are nil, then no result
          ;;(message "no result detected")
          (loop for dict in seognil-dictionaries
                do (progn
                     (insert (concat "[dict: " dict "]<br/> <br/>"))
                     (let ((result (seognil-query-word-definition-in-dict dict word)))
                       (if result
                           (insert result)
                         (insert "No definitions found.")
                         ))
                     (insert "<br/> <br/>")))
          
          (w3m-buffer)
          (switch-to-buffer seognil-buffer-name))))
