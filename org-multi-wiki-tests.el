;;; -*- lexical-binding: t -*-

(require 'buttercup)
(require 'org-multi-wiki)

(describe "Defaults"
  (describe "org-multi-wiki-default-escape-file-name-fn"
    (cl-flet ((escape-fn (heading) (org-multi-wiki-default-escape-file-name-fn heading)))

      (it "does not camel case a single word"
        (let ((result (escape-fn "hello123")))
          (expect result :to-equal "hello123")))

      (it "retain dots"
        (let ((result (escape-fn "github.com")))
          (expect result :to-equal "github.com")))

      (it "retain CJK characters"
        (let ((result (escape-fn "あいうえお雙拼輸入方案")))
          (expect result :to-equal "あいうえお雙拼輸入方案")))

      (it "join multiple words and convert to UpperCamelCase"
        (let ((result (escape-fn "hello world")))
          (expect result :to-equal "HelloWorld")))

      (it "split words by hyphens and underscores as well"
        (let ((result (escape-fn "ab-cd ef_gh")))
          (expect result :to-equal "AbCdEfGh")))

      (it "eliminates most symbols not specified in the above"
        (let ((result (escape-fn "123@#!([]<\\|hello")))
          (expect result :to-equal "123hello")))))

  (describe "org-multi-wiki-default-entry-template-fn"
    (cl-flet ((template (heading) (org-multi-wiki-default-entry-template-fn heading)))

      (it "generates a top-level entry"
        (let ((result (template "Hello")))
          (expect result :to-equal "* Hello\n"))))))
