;;; tiny.el --- Quickly generate linear ranges in Emacs

;; Copyright (C) 2013-2015, 2017  Free Software Foundation, Inc.

;; Author: Oleh Krehel <ohwoeowho@gmail.com>
;; URL: https://github.com/abo-abo/tiny
;; Version: 0.2.1
;; Keywords: convenience

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

;;; Commentary:
;;
;; To set it up, just bind e.g.:
;;
;;     (global-set-key (kbd "C-;") #'tiny-expand)
;;
;; Usage:
;; This extension's main command is `tiny-expand'.
;; It's meant to quickly generate linear ranges, e.g. 5, 6, 7, 8.
;; Some elisp proficiency is an advantage, since you can transform
;; your numeric range with an elisp expression.
;;
;; There's also some emphasis on the brevity of the expression to be
;; expanded: e.g. instead of typing (+ x 2), you can do +x2.
;; You can still do the full thing, but +x2 would save you some
;; key strokes.
;;
;; You can test out the following snippets by positioning the point at
;; the end of the expression and calling `tiny-expand':
;;
;;   m10
;;   m5 10
;;   m5,10
;;   m5 10*xx
;;   m5 10*xx%x
;;   m5 10*xx|0x%x
;;   m25+x?a%c
;;   m25+x?A%c
;;   m97,122(string x)
;;   m97,122stringxx
;;   m97,120stringxupcasex
;;   m97,120stringxupcasex)x
;;   m\n;; 10|%(+ x x) and %(* x x) and %s
;;   m10*2+3x
;;   m\n;; 10expx
;;   m5\n;; 20expx%014.2f
;;   m7|%(expt 2 x)
;;   m, 7|0x%02x
;;   m10|%0.2f
;;   m1\n14|*** TODO http://emacsrocks.com/e%02d.html
;;   m1\n10|convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%s_mono.pdf
;;   (setq foo-list '(m1 11+x96|?%c))
;;   m1\n10listx+x96|convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%c_mono.pdf
;;   m1\n10listxnthxfoo-list|convert img%s.jpg -monochrome -resize 50%% -rotate 180 img%c_mono.pdf
;;   m\n;; 16list*xxx)*xx%s:%s:%s
;;   m\n8|**** TODO Learning from Data Week %(+ x 2) \nSCHEDULED: <%(date "Oct 7" (* x 7))> DEADLINE: <%(date "Oct 14" (* x 7))>
;;
;; As you might have guessed, the syntax is as follows:
;;
;;   m[<range start:=0>][<separator:= >]<range end>[Lisp expr]|[format expr]
;;
;;     x is the default var in the elisp expression.  It will take one by one
;;     the value of all numbers in the range.
;;
;;     | means that elisp expr has ended and format expr has begun.
;;     It can be omitted if the format expr starts with %.
;;     The keys are the same as for format.
;;     In addition %(sexp) forms are allowed.  The sexp can depend on x.
;;
;;   Note that multiple % can be used in the format expression.
;;   In that case:
;;   - if the Lisp expression returns a list, the members of this list
;;     are used in the appropriate place.
;;   - otherwise, it's just the result of the expression repeated as
;;     many times as necessary.
;;
;; Alternatively, if user does not want to type in the "tiny
;; expressions", they can call the `tiny-helper' command that helps
;; construct the "tiny expression", and then expands that.
;;
;; For example, the below two are equivalent:
;;
;;  - Type "m2_9+1*x2"
;;  - M-x tiny-expand
;;
;; OR
;;
;;  - M-x tiny-helper
;;  - 9 RET 2 RET _ RET +1*x2 RET RET (user entry in the interactive prompts)

;;; Code:

(eval-when-compile
  (require 'cl))
(require 'help-fns)
(require 'org)

(defvar tiny-beg nil
  "Last matched snippet start position.")

(defvar tiny-end nil
  "Last matched snippet end position.")

;;;###autoload
(defun tiny-expand ()
  "Expand current snippet.
It polls the expander functions one by one
if they can expand the thing at point.
First one to return a string succeeds.
These functions are expected to set `tiny-beg' and `tiny-end'
to the bounds of the snippet that they matched.
At the moment, only `tiny-mapconcat' is supported.
`tiny-mapconcat2' should be added to expand rectangles."
  (interactive)
  (let ((e (tiny-mapconcat)))
    (when e
      (goto-char tiny-beg)
      (delete-region tiny-beg tiny-end)
      (insert (eval e t)))))

(defun tiny-setup-default ()
  "Setup shortcuts."
  (global-set-key (kbd "C-;") 'tiny-expand))

(defun tiny--strip-\n (str)
  (replace-regexp-in-string "\\\\n" "\n" str))

(defun tiny-mapconcat ()
  "Format output of `tiny-mapconcat-parse'.
Defaults are used in place of null values."
  (let ((parsed (tiny-mapconcat-parse)))
    (when parsed
      (let* ((n0 (string-to-number (or (nth 0 parsed) "0")))
             (n1 (nth 1 parsed))
             (s1 (cond ((null n1)
                        " ")
                       ((equal n1 "m")
                        "")
                       (t
                        n1)))
             (n2 (read (nth 2 parsed)))
             (expr (or (nth 3 parsed) "x"))
             (lexpr (read expr))
             (n-have (if (and (listp lexpr) (eq (car lexpr) 'list))
                         (1- (length lexpr))
                       0))
             (expr (if (zerop n-have) `(list ,lexpr) lexpr))
             (n-have (if (zerop n-have) 1 n-have))
             (tes (tiny-extract-sexps (or (nth 4 parsed) "%s")))
             (fmt (car tes))
             (idx -1)
             (seq (number-sequence n0 n2 (if (>= n0 n2) -1 1))))
        `(mapconcat (lambda (x)
                      (let ((lst ,expr))
                        (format ,(tiny--strip-\n fmt)
                                ,@(mapcar (lambda (x)
                                            (if x
                                                (read x)
                                              (if (>= (1+ idx) n-have)
                                                  'x
                                                `(nth ,(incf idx) lst))))
                                          (cdr tes)))))
                    ',seq
                    ,(tiny--strip-\n s1))))))

(defconst tiny-format-str
  (let ((flags "[+ #-0]\\{0,1\\}")
        (width "[0-9]*")
        (precision "\\(?:\\.[0-9]+\\)?")
        (character "[sdoxXefgcS]?"))
    (format "\\(%s%s%s%s\\)("
            flags width precision character)))

(defun tiny-extract-sexps (str)
  "Return (STR & FORMS).
Each element of FORMS corresponds to a `format'-style % form in STR.

  * %% forms are skipped
  * %(sexp) is replaced with %s in STR, and put in FORMS
  * the rest of forms are untouched in STR, and put as nil in FORMS"
  (let ((start 0)
        forms beg fexp)
    (condition-case nil
        (while (setq beg (string-match "%" str start))
          (setq start (1+ beg))

          (cond ((= ?% (aref str (1+ beg)))
                 (incf start))

                ((and (eq beg (string-match tiny-format-str str beg))
                      (setq fexp (match-string-no-properties 1 str)))
                 (incf beg (length fexp))
                 (destructuring-bind (_sexp . end)
                     (read-from-string str beg)
                   (push
                    (replace-regexp-in-string "(date" "(tiny-date"
                                              (substring str beg end))
                    forms)
                   (setq str (concat (substring str 0 beg)
                                     (if (string= fexp "%") "s" "")
                                     (substring str end)))))
                (t (push nil forms))))
      (error (message "Malformed sexp: %s" (substring str beg))))
    (cons str (nreverse forms))))

(defun tiny-mapconcat-parse ()
  "Try to match a snippet of this form:
m[START][SEPARATOR]END[EXPR]|[FORMAT]

* START     - integer (defaults to 0)
* SEPARATOR - string  (defaults to \" \")
* END       - integer (required)
* EXPR      - Lisp expression: function body with argument x (defaults to x)
  Parens are optional if it's unambiguous:
  - `(* 2 (+ x 3))'   <-> *2+x3
  - `(exp x)'         <-> expx
  A closing paren may be added to resolve ambiguity:
  - `(* 2 (+ x 3) x)' <-> *2+x3)
* FORMAT    - string, `format'-style (defaults to \"%s\")
  | separator can be omitted if FORMAT starts with %.

Return nil if nothing was matched, otherwise
 (START SEPARATOR END EXPR FORMAT)"
  (let ((case-fold-search nil)
        n1 s1 n2 expr fmt str)
    (when (catch 'done
            (cond
              ;; either start with a number
              ((looking-back "\\bm\\(-?[0-9]+\\)\\(.*?\\)"
                             (line-beginning-position))
               (setq n1 (match-string-no-properties 1)
                     str (match-string-no-properties 2)
                     tiny-beg (match-beginning 0)
                     tiny-end (match-end 0))
               (when (zerop (length str))
                 (setq n2 n1
                       n1 nil)
                 (throw 'done t)))
              ;; else capture the whole thing
              ((looking-back "\\bm\\([^%|\n]*[0-9][^\n]*\\)"
                             (line-beginning-position))
               (setq str (match-string-no-properties 1)
                     tiny-beg (match-beginning 0)
                     tiny-end (match-end 0))
               (when (zerop (length str))
                 (throw 'done nil)))
              (t (throw 'done nil)))
            ;; at this point, `str' should be either [sep]<num>[expr][fmt]
            ;; or [expr][fmt]
            ;;
            ;; First, try to match [expr][fmt]
            (string-match "^\\(.*?\\)|?\\(%.*\\)?$" str)
            (setq expr (match-string-no-properties 1 str))
            (setq fmt (match-string-no-properties 2 str))
            ;; If it's a valid expression, we're done
            (when (setq expr (tiny-tokenize expr))
              (setq n2 n1
                    n1 nil)
              (throw 'done t))
            ;; at this point, `str' is [sep]<num>[expr][fmt]
            (if (string-match "^\\([^\n0-9]*?\\)\\(-?[0-9]+\\)\\(.*\\)?$" str)
                (setq s1 (match-string-no-properties 1 str)
                      n2 (match-string-no-properties 2 str)
                      str (match-string-no-properties 3 str))
              ;; here there's only n2 that was matched as n1
              (setq n2 n1
                    n1 nil))
            ;; match expr_fmt
            (unless (zerop (length str))
              (if (or (string-match "^\\([^\n%|]*?\\)|\\([^\n]*\\)?$" str)
                      (string-match "^\\([^\n%|]*?\\)\\(%[^\n]*\\)?$" str))
                  (progn
                    (setq expr (tiny-tokenize (match-string-no-properties 1 str)))
                    (setq fmt (match-string-no-properties 2 str)))
                (error "Couldn't match %s" str)))
            t)
      (when (equal expr "")
        (setq expr nil))
      (list n1 s1 n2 expr fmt))))

;; TODO: check for arity: this doesn't work: exptxy
(defun tiny-tokenize (str)
  "Transform shorthand Lisp expression STR to proper Lisp."
  (if (equal str "")
      ""
    (ignore-errors
      (let ((i 0)
            (j 1)
            (len (length str))
            sym s out allow-spc
            (n-paren 0)
            (expect-fun t))
        (while (< i len)
          (setq s (substring str i j))
          (when (cond
                  ((string= s "x")
                   (push s out)
                   (push " " out))
                  ((string= s " ")
                   (if allow-spc
                       t
                     (error "Unexpected \" \"")))
                  ;; special syntax to read chars
                  ((string= s "?")
                   (setq s (format "%s" (read (substring str i (incf j)))))
                   (push s out)
                   (push " " out))
                  ((string= s ")")
                   ;; expect a close paren only if it's necessary
                   (if (>= n-paren 0)
                       (decf n-paren)
                     (error "Unexpected \")\""))
                   (when (string= (car out) " ")
                     (pop out))
                   (push ")" out)
                   (push " " out))
                  ((string= s "(")
                   ;; open paren is used sometimes
                   ;; when there are numbers in the expression
                   (setq expect-fun t)
                   (incf n-paren)
                   (push "(" out))
                  ((progn (setq sym (intern-soft s))
                          (cond
                            ;; general functionp
                            ((and (not (eq t (help-function-arglist sym)))
                                  (not (eq sym '\,)))
                             (setq expect-fun nil)
                             (setq allow-spc t)
                             ;; (when (zerop n-paren) (push "(" out))
                             (unless (equal (car out) "(")
                               (push "(" out)
                               (incf n-paren))
                             t)
                            ((and sym (boundp sym) (not expect-fun))
                             t)))
                   (push s out)
                   (push " " out))
                  ((numberp (read s))
                   (let* ((num (string-to-number (substring str i)))
                          (num-s (format "%s" num)))
                     (push num-s out)
                     (push " " out)
                     (setq j (+ i (length num-s)))))
                  (t
                   (incf j)
                   nil))
            (setq i j)
            (setq j (1+ i))))
        ;; last space
        (when (string= (car out) " ")
          (pop out))
        (concat
         (apply #'concat (nreverse out))
         (make-string n-paren ?\)))))))

(defun tiny-date (s &optional shift)
  "Return date representation of S.
`org-mode' format is used.
Optional SHIFT argument is the integer amount of days to shift."
  (let* ((ct (decode-time (current-time)))
         (time (apply 'encode-time
                      (org-read-date-analyze
                       s nil
                       ct)))
         (formatter
          (if (equal (cl-subseq ct 1 3)
                     (cl-subseq (decode-time time) 1 3))
              "%Y-%m-%d %a"
            "%Y-%m-%d %a %H:%M")))
    (when shift
      (setq time (time-add time (days-to-time shift))))
    (format-time-string formatter time)))

;;;###autoload
(defun tiny-helper (&optional end-val begin-val sep op fmt)
  "Helper function for `tiny-expand'.

The arguments to this function construct a “tiny expression”
\"mBSEO|F\" where
  E is the end value (END-VAL)     - defaults to 0 internally if nil or \"\",
                                      or 9 if BEGIN-VAL is nil or \"\" too.
  B is the begin value (BEGIN-VAL) - defaults to 0 internally if nil or \"\".
  S is the separator (SEP)         - defaults to \" \" if nil or \"\".
  O is the elisp operation (OP)    - defaults to \"\" if nil.
  F is the format (FMT)            - defaults to \"\" if nil.

If `tiny' expansion is possible at point, do it.
Otherwise activate the helper to generate a valid “tiny
expression” and expand that.

Usage: Call TINY-HELPER, ↵↵↵↵↵            -> 0 1 2 3 4 5 6 7 8 9
       Call TINY-HELPER, 9↵2↵_↵+1*x2↵↵    -> 5_7_9_11_13_15_17_19
       Call TINY-HELPER, 15↵1↵↵-30*2x↵%x↵ -> 1c 1a 18 16 14 12 10 e c a 8 6 4 2 0"
  (interactive
   (unless (or
            ;; Use the helper only if tiny expansion is not
            ;; possible at point and if the buffer is editable.d
            (barf-if-buffer-read-only)
            (tiny-mapconcat))
     (let ((prompt (propertize "tiny-helper: " 'face 'font-lock-function-name-face)))
       (list (read-string (concat prompt
                                  "END value "
                                  "[Hit RET for default=0; "
                                  "Auto-set to 9 if both begin and end values are 0]: "))
             (read-string (concat prompt
                                  "BEGIN value "
                                  "[Hit RET for default=0; "
                                  "Has to be *smaller* than the end value]: "))
             (read-string (concat prompt
                                  "Separator "
                                  "[Hit RET for default=Space; "
                                  "eg: \\n; No math operators like - or = allowed]: "))
             (read-string (concat prompt
                                  "Lisp Operation "
                                  "[Hit RET for default=\"\" (no Lisp operation); "
                                  "Parentheses are optional; eg: *xx | (+ x ?A) | *2+3x]: "))
             (read-string (concat prompt
                                  "Format "
                                  "[Hit RET for default=\"\" (%0d); "
                                  "eg: %x | 0x%x | %c | %s | %(+ x x) | "
                                  "%014.2f | %03d; Parentheses required here for sexps]: "))))))
  (barf-if-buffer-read-only)  ;Proceed only if the buffer is editable.
  ;; Use the helper to derive a "tiny expression" if tiny expansion is
  ;; not possible at point.
  (when (null (tiny-mapconcat))
    (let* ((tiny-key-binding (or (substitute-command-keys "\\[tiny-helper]")
                                 (substitute-command-keys "\\[tiny-expand]")))
           (end-val (if (null end-val) "" end-val)) ;Initialize to empty strings for non-interactive use.
           (begin-val (if (null begin-val) "" begin-val))
           (sep (if (null sep) "" sep))
           (op (if (null op) "" op))
           (fmt (if (null fmt) "" fmt))
           (end-val-num (string-to-number end-val)) ;Note that (string-to-number "") -> 0
           (begin-val-num (string-to-number begin-val))
           tiny-expr)
      ;; BEGIN-VAL and END-VAL sanity check.
      (cond
        ((= end-val-num begin-val-num)
         (if (zerop end-val-num)
             ;; If both are zero, set the end value to 9 (arbitrarily chosen).
             (setq end-val "9")
           (user-error (format "Begin value (%s) and End value (%s) cannot be the same"
                               begin-val end-val))))
        ((< end-val-num begin-val-num)
         (user-error (format "End value (%s) has to be greater than the begin value (%s)"
                             begin-val end-val))))
      ;; SEP cannot be an empty string if BEGIN-VAL is a non-empty string.
      ;; It is OK to not specify BEGIN-VAL if it is 0.
      (when (and (not (string= begin-val ""))
                 (string= sep ""))
        (setq sep " "))
      ;; When non-empty, prefix FMT with the | char for reading clarity.
      (when (not (string= fmt ""))
        (setq fmt (concat "|" fmt)))
      (setq tiny-expr (concat "m" begin-val sep end-val op fmt))
      (message (format "This %s expansion can also be done by typing %s and then %s"
                       (propertize "tiny"
                                   'face 'font-lock-function-name-face)
                       (propertize tiny-expr
                                   'face 'font-lock-keyword-face)
                       (if (stringp tiny-key-binding)
                           (propertize tiny-key-binding
                                       'face 'font-lock-keyword-face)
                         (concat
                          (propertize "M-x tiny-helper"
                                      'face 'font-lock-keyword-face)
                          " or "
                          (propertize "M-x tiny-expand"
                                      'face 'font-lock-keyword-face)))))
      (insert tiny-expr)
      (undo-boundary)))
  (tiny-expand))


(provide 'tiny)
;;; tiny.el ends here
