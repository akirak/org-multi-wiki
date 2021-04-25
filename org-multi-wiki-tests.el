;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-multi-wiki)

(describe "Defaults"
  (describe "org-multi-wiki-escape-file-name-camelcase-1"
    (cl-flet ((escape-fn (heading) (org-multi-wiki-escape-file-name-camelcase-1 heading)))

      (it "does not camel case a single word"
        (let ((result (escape-fn "hello123")))
          (expect result :to-equal "hello123")))

      (it "retain dots"
        (let ((result (escape-fn "github.com")))
          (expect result :to-equal "github.com")))

      (it "retain CJK characters"
        (let ((result (escape-fn "あいうえお雙拼輸入方案")))
          (expect result :to-equal "あいうえお雙拼輸入方案")))

      (it "join multiple words and upcase initials"
        (let ((result (escape-fn "hello world")))
          (expect result :to-equal "HelloWorld"))
        (let ((result (escape-fn "hello WORLD")))
          (expect result :to-equal "HelloWORLD"))
        (let ((result (escape-fn "hello WORLD1")))
          (expect result :to-equal "HelloWORLD1"))
        (let ((result (escape-fn "hello wOrld")))
          (expect result :to-equal "HelloWOrld")))

      (it "Don't treat hyphens and underscores as word separators"
        (let ((result (escape-fn "org-refile")))
          (expect result :to-equal "org-refile"))
        (let ((result (escape-fn "foo_id_1")))
          (expect result :to-equal "foo_id_1")))

      (it "eliminates words like a, an, and the"
        (let ((result (escape-fn "This is a pen")))
          (expect result :to-equal "ThisIsPen"))
        (let ((result (escape-fn "This is the knife")))
          (expect result :to-equal "ThisIsKnife")))

      (it "eliminates most symbols not specified in the above"
        (let ((result (escape-fn "123@#!([]<\\|hello")))
          (expect result :to-equal "123hello")))

      (it "Don't eliminate slash"
        (let ((result (escape-fn "hello/john connor")))
          (expect result :to-equal "hello/JohnConnor")))))

  (describe "org-multi-wiki-default-custom-id-escape-fn"
    (cl-flet ((escape-fn (heading) (org-multi-wiki-default-custom-id-escape-fn heading)))

      (it "Converts to lower case"
        (let ((result (escape-fn "This is an apple")))
          (expect result :to-equal "this-is-an-apple")))

      (it "Splits words by spaces, hyphens, and underscores"
        (let ((result (escape-fn "org-multi-wiki is an Emacs Lisp package")))
          (expect result :to-equal "org-multi-wiki-is-an-emacs-lisp-package"))
        (let ((result (escape-fn "Elixir uses under_scores in symbols")))
          (expect result :to-equal "elixir-uses-under-scores-in-symbols")))

      (it "Eliminates other symbols"
        (let ((result (escape-fn "I was surprised to see it coming!")))
          (expect result :to-equal "i-was-surprised-to-see-it-coming")))))

  (describe "org-multi-wiki-default-entry-template-fn"
    (cl-flet ((template (heading) (org-multi-wiki-default-entry-template-fn heading)))

      (it "generates a top-level entry"
        (let ((result (template "Hello")))
          (expect result :to-equal "* Hello\n"))))))

(describe "org-multi-wiki--org-extension"
  (it "returns the extension of an Org file"
    (expect (org-multi-wiki--org-extension "sample.org")
            :to-equal ".org")
    (expect (org-multi-wiki--org-extension "sample.org.gpg")
            :to-equal ".org.gpg"))
  (it "returns nil for other files"
    (expect (org-multi-wiki--org-extension "sample.txt")
            :to-be nil)))

(describe "org-multi-wiki--strip-org-extension"
  (it "strips .org"
    (expect (org-multi-wiki--strip-org-extension "sample.org")
            :to-equal "sample"))
  (it "strips .org.gpg"
    (expect (org-multi-wiki--strip-org-extension "sample.org.gpg")
            :to-equal "sample"))
  (it "retains the original"
    (expect (org-multi-wiki--strip-org-extension "sample.txt")
            :to-equal "sample.txt")
    (expect (org-multi-wiki--strip-org-extension "hello.org.nonorg")
            :to-equal "hello.org.nonorg")))

(describe "org-multi-wiki--extra-files"
  (it "returns a flat list of file names"
    ;; Note that I am going to remove notes.org in the future, so I
    ;; will have to update this test case as well then.
    (expect (let ((org-directory ".")
                  (org-agenda-files (list "."))
                  (org-multi-wiki-extra-files
                   '("."
                     "NON-EXISTENT-FILE"
                     "NON-EXISTENT-DIR/"
                     org-directory
                     org-agenda-files)))
              (org-multi-wiki--extra-files))
            :to-equal
            (->> (-map (lambda (f)
                         (expand-file-name f default-directory))
                       '("README.org" "notes.org"))
                 (-repeat 3)
                 (-flatten-n 1)))))
