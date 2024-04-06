;;; treesitedit.el --- Paredit-inspired navigation on top of treesitter.
;;;
;;; Commentary:
;;;
;;; Code:

(require 'treesit)


;;;; Motion: forward and backward


(defun treesitedit-backward (&optional arg)
  "Move point backward visiting node start positions.

Repeat ARG times.

Behave like `treesitedit-forward' if ARG is negative."
  (interactive "P")
  (let ((n (or arg 1)))
    (cond ((< 0 n) (dotimes (i n)     (treesitedit--move #'treesitedit--move-backward)))
          ((< n 0) (dotimes (i (- n)) (treesitedit--move #'treesitedit--move-forward))))))


(defun treesitedit-forward (&optional arg)
  "Move point forward visiting node end positions.

Repeat ARG times.

Behave like `treesitedit-backward' if ARG is negative."
  (interactive "P")
  (let ((n (or arg 1)))
    (cond ((< 0 n) (dotimes (i n)     (treesitedit--move #'treesitedit--move-forward)))
          ((< n 0) (dotimes (i (- n)) (treesitedit--move #'treesitedit--move-backward))))))


(defun treesitedit--move (fn)
  (goto-char (funcall fn (point))))


(defun treesitedit--move-backward (p)
  (let* ((n (treesitedit--topmost-node p)))
    (while (and n (or (>= (treesit-node-start n) p)
                      (treesitedit--skipped-node-p n)))
      (setq n (or (treesit-node-prev-sibling n)
                  (treesit-node-parent n))))
    (when n (treesit-node-start n))))


(defun treesitedit--move-forward (p)
  (let* ((n (treesitedit--topmost-node p)))
    (while (and n (or (<= (treesit-node-end n) p)
                      (treesitedit--skipped-node-p n)))
      (setq n (or (treesit-node-next-sibling n)
                  (treesit-node-parent n))))
    (when n (treesit-node-end n))))


;;;; Motion: up and down


(defun treesitedit-backward-down (&optional argument)
  "Move backward down into a node.
With a positive argument, move backward down that many levels.
With a negative argument, move forward down that many levels."
  (interactive "P")
  (goto-char (treesitedit--up-down (- 0 (or argument +1)) -1 (point))))


(defun treesitedit-backward-up (&optional argument)
  "Move backward up out of the enclosing node.
With a positive argument, move backward up that many levels.
With a negative argument, move forward up that many levels.
If in a string initially, that counts as one level."
  (interactive "P")
  (goto-char (treesitedit--up-down (- 0 (or argument +1)) +1 (point))))


(defun treesitedit-forward-down (&optional argument)
  "Move forward down into a node.
With a positive argument, move forward down that many levels.
With a negative argument, move backward down that many levels."
  (interactive "P")
  (goto-char (treesitedit--up-down (or argument +1) -1 (point))))


(defun treesitedit-forward-up (&optional argument)
  "Move forward up out of the enclosing node.
With a positive argument, move forward up that many levels.
With a negative argument, move backward up that many levels.
If in a string initially, that counts as one level."
  (interactive "P")
  (goto-char (treesitedit--up-down (or argument +1) +1 (point))))


(defun treesitedit--up-down (signed-levels vertical-direction p)
  "Calculate target position for motions inspired by `paredit'.

SIGNED-LEVELS tells how many levels to go up or down, sign
indicates forward or back.

VERTICAL-DIRECTION tells to down if negative, up if positive.

P is the starting position"
  (let ((levels (abs signed-levels)))
    (dotimes (j levels)
      (setq p (treesitedit--up-down-step signed-levels vertical-direction p)))
    p))


(defun treesitedit--up-down-step (horiz-direction vertical-direction p)
  (let* ((backward (< horiz-direction 0))
         (forward (not backward))
         (down (< vertical-direction 0))
         (up (not down))
         (n (treesitedit--topmost-node p)))
    (cond
     ((and backward up)
      ;; simply go to the start of the parent node
      (treesit-node-start (or (treesit-node-parent n) n)))
     ((and forward up)
      ;; simply go to the end of the parent node
      (treesit-node-end (or (treesit-node-parent n) n)))
     ((and forward down)
      ;; search self and next siblings for children
      (let ((nn (seq-find
                 (lambda (c) (> (treesit-node-start c) p))
                 (seq-mapcat #'treesitedit--node-with-descendants
                             (treesitedit--node-with-next-siblings n)))))
        (if nn (treesit-node-start nn)
          (error "forward down motion found no suitable nodes"))))
     ((and backward down)
      ;; go backward by sibling until one with children, then go to the end of the last child
      (while (and n (not (and (> (treesit-node-child-count n) 0)
                              (< (treesit-node-end (treesit-node-child n -1)) p))))
        (setq n (treesit-node-prev-sibling n)))
      (if n (treesit-node-end (treesit-node-child n -1))
        (error "backward down motion found no suitable nodes"))))))


;;;; Skipped nodes


(defun treesitedit--skipped-node-p (n)
  (not (null (string-match-p (rx string-start (* (or space "\n")) string-end)
                             (treesit-node-text n)))))


;;;; Extending treesit stdlib


(defun treesitedit--node-with-descendants (node)
  "Return a list including NODE and all descendants."
  (let ((r (list)))
    (treesit-search-subtree node (lambda (c)
                                   (setq r (cons c r))
                                   nil))
    (reverse r)))


(defun treesitedit--node-with-next-siblings (node)
  "Return a list including NODE and all subsequent siblings."
  (let ((n node)
        (r (list)))
    (while n
      (setq r (cons n r))
      (setq n (treesit-node-next-sibling n)))
    (reverse r)))


(defun treesitedit--topmost-node (pos)
  "Finds the top-most node at POS position."
  (let ((x (treesit-node-at pos)))
    (while (let ((p (treesit-node-parent x)))
             (and p (equal (treesit-node-start p)
                           (treesit-node-start x))))
      (setq x (treesit-node-parent x)))
    x))


(provide 'treesitedit)
;;; treesitedit.el ends here
