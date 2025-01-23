;;; treesitedit.el --- Paredit-inspired navigation on top of treesitter.  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:
;;;
;;; Code:

(require 'treesit)

;;;; Moving in the Block Node Tree Structure
;;;;
;;;; The four motions mimic built-in Moving by Parentheses motions. Instead of groups of balanced parentheses, they
;;;; operate on block nodes. Each language defines which treesitter nodes are blocks.
;;;;
;;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-by-Parens.html


(defcustom treesitedit-block-nodes
  (let ((go-rx (rx bol (or
                        "argument_list"
                        "assignment_statement"
                        "expression_statement"
                        "composite_literal"
                        "for_statement"
                        "func_literal"
                        "function_declaration"
                        "if_statement"
                        "interpreted_string_literal"
                        "method_declaration"
                        "parameter_list"
                        "return_statement"
                        "short_var_declaration"
                        "type_declaration"
                        "var_declaration")
                   eol)))
    `((go-mode . ,go-rx)
      (go-ts-mode . ,go-rx)))
  "Define which treesitter nodes are considered to be block nodes by mode."
  :type 'custom
  :group 'treesitedit)


(defun treesitedit-forward-list (arg)
  "Move forward over a block node.

Stay at the same level in the block node tree structure.

Fail if no movement at the current level is possible.

A positive ARG serves as a repeat count; a negative ARG reverses
the direction of motion."
  (interactive "P")
  (setq arg (or (if (equal arg '-) -1 arg) 1))
  (dotimes (_ (abs arg))
    (if (> arg 0)
        (treesitedit--forward-list-1)
      (treesitedit--backward-list-1))))


(defun treesitedit-backward-list (arg)
  "Move with `treesitedit-forward-list' negated ARG times."
  (interactive "P")
  (setq arg (or (if (equal arg '-) -1 arg) 1))
  (dotimes (_ (abs arg))
    (if (> arg 0)
        (treesitedit--backward-list-1)
      (treesitedit--forward-list-1))))


(defun treesitedit-backward-up-list (arg)
  "Move up the block node tree ARG levels.

Moves backwards by default. Negating the ARG reverses the
direction to move up and forwards."
  (interactive "P")
  (setq arg (or (if (equal arg '-) -1 arg) 1))
  (dotimes (_ (abs arg))
    (if (< arg 0)
        (treesitedit--forward-up-list-1)
      (treesitedit--backward-up-list-1))))


(defun treesitedit-down-list (arg)
  "Move forward and down the block node tree ARG levels.

Negating the ARG reverses the direction to move backwards."
  (interactive "P")
  (setq arg (or (if (equal arg '-) -1 arg) 1))
  (dotimes (_ (abs arg))
    (if (> arg 0)
        (treesitedit--forward-down-list-1)
      (treesitedit--backward-down-list-1))))


(defun treesitedit--forward-list-1 ()
  "Implement one step of forward motion."
  (pcase (treesitedit--position)
    (`(before-first ,c ,_)
     (goto-char (treesit-node-end c)))
    (`(between ,_ ,c2 ,_)
     (goto-char (treesit-node-end c2)))
    (_
     (message "%s" "No next group"))))


(defun treesitedit--backward-list-1 ()
  "Implement one step of backward motion."
  (pcase (treesitedit--position)
    (`(between ,c1 ,_ ,_)
     (goto-char (treesit-node-start c1)))
    (`(after-last ,c ,_)
     (goto-char (treesit-node-start c)))
    (_
     (message "%s" "No previous group"))))


(defun treesitedit--backward-down-list-1 ()
  "Implement one step of backward-down motion."
  (let ((n (pcase (treesitedit--position)
             (`(between ,c1 ,_ ,_) c1)
             (`(after-last ,c ,_) c)
             (_ (error "%s" "At bottom level")))))
    (let ((cc (treesitedit--children n)))
      (if (null cc)
          (message "%s" "At bottom level")
        (goto-char (treesit-node-end (car (reverse cc))))))))


(defun treesitedit--forward-down-list-1 ()
  "Implement one step of forward-down motion."
  (let ((n
         (pcase (treesitedit--position)
           (`(before-first ,c ,_) c)
           (`(between ,_ ,c2 ,_) c2)
           (_ (error "%s" "At bottom level")))))
    (goto-char (treesit-node-start n))
    (treesitedit--forward-1)))


(defun treesitedit--backward-up-list-1 ()
  "Implement one step of backward-up motion."
  (let ((p (treesitedit--parent)))
    (if p (goto-char (treesit-node-start p))
      (message "%s" "At top level"))))


(defun treesitedit--forward-up-list-1 ()
  "Implement one step of forward-up motion."
  (let ((p (treesitedit--parent)))
    (if p (goto-char (treesit-node-end p))
      (message "%s" "At top level"))))


(defun treesitedit--position ()
  "Determine current position in the block node tree.

It should be one of four cases under parent p:

1. (between c1 c2 p)
2. (before-first c2 p)
3. (after-last c1 p)
4. (under p)

The last case indicates that the parent node has no block node
children."
  (let ((p (treesitedit--parent))
        (x (point))
        (ch nil)
        (ix nil))
    (if (null p) nil

      ;; paired blocky children of p
      (setq ch (seq-filter
                #'treesitedit--block-p
                (treesitedit--children p)))

      ;; count nodes preceding point
      (setq ix (seq-length (seq-take-while (lambda (n)
                                             (> x (treesit-node-start n)))
                                           ch)))
      (cond
       ((null ch)
        `(under ,p))
       ((equal ix 0)
        `(before-first ,(seq-first ch) ,p))
       ((equal ix (seq-length ch))
        `(after-last ,(seq-elt ch (- ix 1)) ,p))
       (t
        `(between
          ,(seq-elt ch (- ix 1))
          ,(seq-elt ch ix)
          ,p))))))


(defun treesitedit--block-p (n)
  "Test a treesitter node N to see if it is a block node."
  (or (null (treesit-node-parent n))
      (string-match-p (alist-get major-mode treesitedit-block-nodes (rx bol eol))
                      (treesit-node-type n))))


(defun treesitedit--children (n)
  "Find all block children of a treesitter node N."
  (let ((cc nil))
    (dolist (c (treesit-node-children n))
      (if (treesitedit--block-p c)
          (setq cc (cons c cc))
        (setq cc (append (reverse (treesitedit--children c)) cc))))
    (reverse cc)))


(defun treesitedit--parent ()
  "Find current block parent node."
  (let ((x (point)))
    (treesit-parent-until
     (treesit-node-at x)
     (lambda (n)
       (and (> x (treesit-node-start n))
            (< x (treesit-node-end n))
            (treesitedit--block-p n))))))


;;;; Motion: forward and backward


(defun treesitedit-backward (&optional arg)
  "Move point backward visiting node start positions.

Repeat ARG times.

Behave like `treesitedit-forward' if ARG is negative."
  (interactive "P")
  (setq arg (or (if (equal arg '-) -1 arg) 1))
  (dotimes (_ (abs arg))
    (if (< arg 0)
        (treesitedit--forward-1)
      (treesitedit--backward-1))))


(defun treesitedit-forward (&optional arg)
  "Move point forward visiting node end positions.

Repeat ARG times.

Behave like `treesitedit-backward' if ARG is negative."
  (interactive "P")
  (setq arg (or (if (equal arg '-) -1 arg) 1))
  (dotimes (_ (abs arg))
    (if (> arg 0)
        (treesitedit--forward-1)
      (treesitedit--backward-1))))


(defun treesitedit--forward-1 ()
  "One step forward."
  (let* ((p (point))
         (cn (treesit-node-at p))
         (nn nil))
    (cond
     ((and (> (treesit-node-end cn) p)
           (not (treesitedit--whitespace-nodep cn)))
      (goto-char (treesit-node-end cn)))
     (t
      (setq nn (treesit-search-forward
                cn (lambda (x)
                     (and
                      (not (treesitedit--whitespace-nodep x))
                      (> (treesit-node-end x) p)))
                nil 'all))
      (when nn
        (goto-char (treesit-node-end nn)))))))


(defun treesitedit--backward-1 ()
  "One step backward."
  (let* ((p (point))
         (cn (treesit-node-at p))
         (nn nil))
    (cond
     ((and (< (treesit-node-start cn) p)
           (not (treesitedit--whitespace-nodep cn)))
      (goto-char (treesit-node-start cn)))
     (t
      (setq nn (treesit-search-forward
                cn (lambda (x)
                     (and
                      (not (treesitedit--whitespace-nodep x))
                      (< (treesit-node-start x) p)))
                'backward 'all))
      (when nn
        (goto-char (treesit-node-start nn)))))))


(defun treesitedit--whitespace-nodep (node)
  "Check if NODE is a white-space node."
  (equal "" (string-clean-whitespace (treesit-node-type node))))


;;;; Marking


(defun treesitedit-mark-sexp ()
  "Mark and select the current node around point.

If region is active, extend the selection.

Use `exchange-point-and-mark' to reverse the direction in which
selection is growing.

Inspired by meow-edit/meow and magnars/expand-region."
  (interactive)
  (let ((f (if (and (region-active-p)
                    (< (point) (mark)))
               'treesitedit-backward-list
             'treesitedit-forward-list)))
    (unless (region-active-p)
      (set-mark (treesit-node-start (treesit-node-at (point)))))
    (call-interactively f)))


;;;; Killing (terrible legacy Emacs term)


(defun treesitedit-kill-sexp (&optional arg)
  "Kill from point to ARG nodes forward."
  (interactive "P")
  (let ((p0 (point)))
    (treesitedit-forward-list (or arg 1))
    (kill-region p0 (point))))


;;;; Mode definition


(defvar treesitedit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "<remap> <forward-sexp>") #'treesitedit-forward)
    (define-key m (kbd "<remap> <backward-sexp>") #'treesitedit-backward)
    (define-key m (kbd "<remap> <backward-up-list>") #'treesitedit-backward-up-list)
    (define-key m (kbd "<remap> <down-list>") #'treesitedit-down-list)
    (define-key m (kbd "<remap> <forward-list>") #'treesitedit-forward-list)
    (define-key m (kbd "<remap> <backward-list>") #'treesitedit-backward-list)
    (define-key m (kbd "<remap> <kill-sexp>") #'treesitedit-kill-sexp)
    (define-key m (kbd "<remap> <mark-sexp>") #'treesitedit-mark-sexp)
    m)
  "Keymap for the treesitedit minor mode.")


;;;###autoload
(define-minor-mode treesitedit-mode
  "Minor mode for making editing motions better with treesitter.")


(provide 'treesitedit)
;;; treesitedit.el ends here
