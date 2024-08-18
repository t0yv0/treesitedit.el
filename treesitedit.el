;;; treesitedit.el --- Paredit-inspired navigation on top of treesitter.  -*- lexical-binding:t -*-
;;;
;;; Version: 1
;;;
;;; Commentary:
;;;
;;; Code:

(require 'treesit)

;;;; State: current node

;;;;; Non-LISP languages may have several nodes starting at the same point, and walking the node tree up and down may
;;;;; not be moving point. State is needed to track which node at point is considered to be the current node.


(defvar treesitedit--current-node-state nil
  "Current node or NIL.")


(defun treesitedit--current-node ()
  "Determine the current node at POINT."
  (unless (treesitedit--got-current-node-p)
    (setq treesitedit--current-node-state (treesitedit--topmost-node-starting-at (point))))
  treesitedit--current-node-state)


(defun treesitedit--set-current-node (node)
  "Set NODE as the current node."
  (setq treesitedit--current-node-state node)
  (pulse-momentary-highlight-region
     (treesit-node-start node)
     (treesit-node-end node)))


(defun treesitedit--got-current-node-p ()
  "Check if there is an active current node."
  (condition-case nil
      (and treesitedit--current-node-state
           (or (equal (treesit-node-start treesitedit--current-node-state) (point))
               (equal (treesit-node-end treesitedit--current-node-state) (point))))
    (error nil)))


;;;; Motion: forward and backward


(defun treesitedit-backward (&optional arg)
  "Move point backward visiting node start positions.

Repeat ARG times.

Behave like `treesitedit-forward' if ARG is negative."
  (interactive "P")
  (treesitedit--move (- (or arg 1))))


(defun treesitedit-forward (&optional arg)
  "Move point forward visiting node end positions.

Repeat ARG times.

Behave like `treesitedit-backward' if ARG is negative."
  (interactive "P")
  (treesitedit--move (or arg 1)))


(defun treesitedit--move (dx)
  "Move forward if DX is positive and backward otherwise.

Repeat (abs DX) times."
  (let ((cn (treesitedit--current-node))
        (nn nil))
    (dotimes (_ (abs dx))
      (setq nn (treesitedit--node-move cn dx))
      (when nn
        (setq cn nn)
        (goto-char
         (if (< dx 0)
             (treesit-node-start cn)
           (treesit-node-end cn)))))
    (treesitedit--set-current-node cn)))


(defun treesitedit--node-move (node dx)
  "Compute the next node after NODE in DX direction."
  (if (> dx 0)
      (or
       (and (> (treesit-node-end node) (point)) node)
       (treesit-node-next-sibling node)
       (treesitedit--node-parent node))
    (or
     (and (< (treesit-node-start node) (point)) node)
     (treesit-node-prev-sibling node)
     (treesitedit--node-parent node))))


;;;; Motion: diagonals

;;;;; This style of motion is inspired by `paredit'. See http://danmidwood.com/content/2014/11/21/animated-paredit.html
;;;;; for useful animations to visualize diagonal motion.


(defun treesitedit-backward-down (&optional argument)
  "Move backward down into a node.
With a positive ARGUMENT, move backward down that many levels.
With a negative ARGUMENT, move forward down that many levels."
  (interactive "P")
  (treesitedit--up-down (- 0 (or argument +1)) -1))


(defun treesitedit-backward-up (&optional argument)
  "Move backward up out of the enclosing node.
With a positive ARGUMENT, move backward up that many levels.
With a negative ARGUMENT, move forward up that many levels."
  (interactive "P")
  (treesitedit--up-down (- 0 (or argument +1)) +1))


(defun treesitedit-forward-down (&optional argument)
  "Move forward down into a node.
With a positive ARGUMENT, move forward down that many levels.
With a negative ARGUMENT, move backward down that many levels."
  (interactive "P")
  (treesitedit--up-down (or argument +1) -1))


(defun treesitedit-forward-up (&optional argument)
  "Move forward up out of the enclosing node.
With a positive ARGUMENT, move forward up that many levels.
With a negative ARGUMENT, move backward up that many levels."
  (interactive "P")
  (treesitedit--up-down (or argument +1) +1))


(defun treesitedit--up-down (signed-levels vertical-direction)
  "Move point diagonally.

SIGNED-LEVELS tells how many levels to go up or down, sign
indicates forward or back.

VERTICAL-DIRECTION tells to go down if negative, up if positive."
  (let ((dx (if (> signed-levels 0) +1 -1))
        (dy (if (< vertical-direction 0) +1 -1))
        (cn (treesitedit--current-node))
        (nn nil))
    (dotimes (_ (abs signed-levels))
      (setq nn (treesitedit--diagonal-node-move cn dx dy))
      (when nn
        (setq cn nn)))
    (goto-char
     (if (equal dx dy)
         (treesit-node-start cn)
       (treesit-node-end cn)))
    (treesitedit--set-current-node cn)))


(defun treesitedit--diagonal-node-move (node dx dy)
  "Move diagonally starting with NODE.

Return the next node in the given direction, or NIL.

Positive DX: forward, negative: backward.

Positive DY: down, negative: up."
  (let ((n node) (moving t) (nn nil) (fn nil))
    (while moving
      ;; try to move vertically
      (setq nn (if (> dy 0)
                   (treesitedit--node-child n (if (> dx 0) 0 -1))
                 (treesitedit--node-parent n)))
      ;; success terminates the loop
      (when nn
        (setq fn nn)
        (setq moving nil))
      (when moving
        ;; try to move horizontally
        (setq nn (if (> dx 0)
                     (treesit-node-next-sibling n)
                   (treesit-node-prev-sibling n)))
        ;; failure terminates the loop
        (when (not nn)
          (setq moving nil)))
      (setq n nn))
    fn))


;;;; Point to current node


(defun treesitedit--topmost-node-starting-at (pos)
  "Find the top-most node starting at POS position.

If none is found, returns the current node at POS."
  (or (treesit-parent-while (treesit-node-at pos)
                            (lambda (p)
                              (equal (treesit-node-start p) pos)))
      (treesit-node-at pos)))


;;;; Generalized parent-child relation


(defun treesitedit--nodes-overlap-p (n1 n2)
  "Check if N1 and N2 nodes manage the same region."
  (and n1 n2
       (equal (treesit-node-start n1)
              (treesit-node-start n2))
       (equal (treesit-node-end n1)
              (treesit-node-end n2))))


(defun treesitedit--node-parent (node)
  "Find closest non-overlapping parent of NODE."
  (treesit-parent-until
   node
   (lambda (p)
     (not
      (treesitedit--nodes-overlap-p p node)))))


(defun treesitedit--node-child (node ix)
  "Find child IX of NODE skipping overlapping nodes."
  (let ((n node))
    (while (treesitedit--nodes-overlap-p
            n
            (treesit-node-child n 0))
      (setq n (treesit-node-child n 0)))
    (treesit-node-child n ix)))


;;;; Marking


(defun treesitedit-mark-sexp ()
  "Mark and select the current node around point.

If region is active, extend the selection.

Use `exchange-point-and-mark' to reverse the direction in which
selection is growing.

Inspired by meow-edit/meow and magnars/expand-region."
  (interactive)
  (cond
   ((region-active-p)
    (if (> (point) (mark))
        (treesitedit-forward)
      (treesitedit-backward)))
   (t
    (let ((n (treesitedit--current-node)))
      (set-mark (treesit-node-start n))
      (goto-char (treesit-node-end n))))))


;;;; Killing (terrible legacy Emacs term)


(defun treesitedit-kill-sexp (&optional arg)
  "Kill from point to ARG nodes forward."
  (interactive "P")
  (let ((p0 (point)))
    (treesitedit-forward (or arg 1))
    (kill-region p0 (point))))


;;;; Mode definition


(defvar treesitedit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-M-f") #'treesitedit-forward)
    (define-key m (kbd "C-M-b") #'treesitedit-backward)
    (define-key m (kbd "C-M-u") #'treesitedit-backward-up)
    (define-key m (kbd "C-M-d") #'treesitedit-forward-down)
    (define-key m (kbd "C-M-p") #'treesitedit-backward-down)
    (define-key m (kbd "C-M-n") #'treesitedit-forward-up)
    (define-key m (kbd "<remap> <kill-sexp>") #'treesitedit-kill-sexp)
    (define-key m (kbd "<remap> <mark-sexp>") #'treesitedit-mark-sexp)
    m)
  "Keymap for the treesitedit minor mode.")


;;;###autoload
(define-minor-mode treesitedit-mode
  "Minor mode for enabling Paredit-like movement commands.

Uses treesitter to accurately understand the parse tree." )


(provide 'treesitedit)
;;; treesitedit.el ends here
