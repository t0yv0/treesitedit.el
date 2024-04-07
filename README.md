# treesitedit.el

[Paredit](https://paredit.org) inspired navigation on top of treesitter.

This package is under development. The author is using it for editing Go with some occasional TypeScript and Python.

If you are looking for something more mature, try [mickeynp/combobulate](https://github.com/mickeynp/combobulate).

## Usage

### Motion

Following paredit three motion pairs are available:

1. Moving over nodes forward and backward:

    ```
    C-M-b treesitedit-backward         C-M-f treesitedit-forward
    ```

2. Moving back and up out of nodes and forward down into nodes:

    ```
    C-M-u treesitedit-backward-up      C-M-d treesitedit-forward-down
    ```

3. Moving forward and out of nodes and backwards down into nodes. These are slightly less intuitive but appear useful.
   Following paredit these replace the traditional Emacs bindings for forward-list and backward-list:

    ```
    C-M-p treesitedit-backward-down    C-M-n treesitedit-forward-up
    ```

The key design difficulty for languages like Go is that unlike in LISP, a single buffer position corresponds to multiple
nodes. For example, having point at the `f` of `func` has several overlapping nodes such as the `func` literal and the
entire function definition. The motion commands takes a guess which node to operate on, generally picking the mode with
the widest extent.

### Marking

Marking design is simple and inspired by [meow](https://github.com/meow-edit/meow) and
[expand-region](https://github.com/magnars/expand-region.el). `C-M-@` or `C-M-SPC` marks the node around point.
Subsequently pressing this key extends active region forward. To extend it backward, use `C-x C-x` to
`exchange-point-and-mark`.
