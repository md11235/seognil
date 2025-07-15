;; make the content inside WORDS-LIST-FILENAME be like:
;; ("word-1" "word-2" "word-3"), a list containing the words to be
;; tested(queried) in the dictionary DICTIONARY-NAMEn
(defun seognil-test-query-words-in-dict (words-list-filename dicitonary-name)
  (with-temp-buffer
    (insert-file-contents words-list-filename)
    (let ((words-list (read (buffer-substring-no-properties (point-min)
                                                            (point-max)))))
      (loop for word in words-list
            do (cl-assert ((lambda (word)
                             (let ((result (seognil-word-definition-position dicitonary-name word)))
                               (and (not (null result))
                                    (string-equal word
                                                  (car result))))) word)
                          t
                          "No index for %s is found."
                          word)))))

;; run the test using
;; (seognil-test-query-words-in-dict "adict-words-list.txt" "ADict")



