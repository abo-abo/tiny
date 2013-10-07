(defun tiny-extract-sexps-test ()
  (equal
   (tiny-extract-sexps "expr1 %(+ x x), nothing %%  char %c, hex %x, and expr2 %(* x x), float %0.2f and sym %s")
   '("expr1 %s, nothing %%  char %c, hex %x, and expr2 %s, float %0.2f and sym %s"
    "(+ x x)" nil nil "(* x x)" nil nil)))

(defun tiny-mapconcat-parse-test ()
  (let* ((tests
          '(("m10" (nil nil "10" nil nil))
            ("m5%x" (nil nil "5" nil "%x"))
            ("m5 10" ("5" " " "10" nil nil))
            ("m5,10" ("5" "," "10" nil nil))
            ("m5 10*xx" ("5" " " "10" "(* x x)" nil))
            ("m5 10*xx%x" ("5" " " "10" "(* x x)" "%x"))
            ("m5 10*xx|0x%x" ("5" " " "10" "(* x x)" "0x%x"))
            ("m25+x?a%c" (nil nil "25" "(+ x 97)" "%c"))
            ("m25+x?A%c" (nil nil "25" "(+ x 65)" "%c"))
            ("m97,122stringx" ("97" "," "122" "(string x)" nil))
            ("m97,122stringxx" ("97" "," "122" "(string x x)" nil))
            ("m97,120stringxupcasex" ("97" "," "120" "(string x (upcase x))" nil))
            ("m97,120stringxupcasex)x" ("97" "," "120" "(string x (upcase x) x)" nil))
            ("m\\n;; 10|%(+ x x) and %(* x x) and %s"
             (nil "\\n;; " "10" nil "%(+ x x) and %(* x x) and %s"))))
         (fails (cl-remove-if
                 (lambda (test)
                   (equal (cadr test)
                          (with-temp-buffer
                            (insert (car test))
                            (tiny-mapconcat-parse))))
                 tests)))
    (when fails
      (message "`tiny-test' fails %s" fails))))
;; (tiny-mapconcat-parse-test)
