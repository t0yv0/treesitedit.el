(require 'ert)
(require 'treesitedit)

(ert-deftest ert-test-treesitedit-down-list ()
  (ert-test-erts-file "down-list.erts"
                      (lambda ()
                        (go-ts-mode)
                        (re-search-forward (rx "^"))
                        (delete-backward-char 1)
                        (call-interactively 'treesitedit-down-list)
                        (insert "^"))))


(ert-deftest ert-test-treesitedit-forward-list ()
  (ert-test-erts-file "forward-list.erts"
                      (lambda ()
                        (go-ts-mode)
                        (re-search-forward (rx "^"))
                        (delete-backward-char 1)
                        (call-interactively 'treesitedit-forward-list)
                        (insert "^"))))
