# treesitedit.el

[Paredit](https://paredit.org) inspired navigation on top of treesitter.

This package is under development. The author is using it for editing Go with some occasional TypeScript and Python.

## Usage

```emacs-lisp
(require 'treesitedit)
(treesitedit-mode)
```

### Moving up and down

Use `C-M-u` and `C-M-d` to move the current node up and down the tree. Unlike LISP, languages like Go may have several
levels of nodes starting at the same location. Consider the following program, and imagine that point is at the
beginning of `return` (marked with `v`), and the current node is `return m.n*i + j` (marked with `^---^`).

``` go
func (m *matrix) index(i, j int) int {
//  v
    return m.n*i + j
//  ^--------------^
}
```

Pressing `C-M-d` goes down without moving point, giving:

``` go
func (m *matrix) index(i, j int) int {
//  v
    return m.n*i + j
//  ^----^
}
```

To give some visual feedback, `treesitedit` will flash the current node.

### Moving forward and backward

Horizontal motion is accomplished with the forward and backward commands:

```
C-M-b treesitedit-backward
C-M-f treesitedit-forward
```

This motion steps over the nodes at the current level. For example:

``` go
func (m *matrix) index(i, j int) int {
//  v
    return m.n*i + j
//  ^--------------^
}
```

In this position, `C-M-f` jump over the current node and move point to its end:

``` go
func (m *matrix) index(i, j int) int {
//                  v
    return m.n*i + j
//  ^--------------^
}
```

To move point to the end of the `return` keyword, the user might want to go down a level `C-M-d` first and then perform
`C-M-f` at that level.

### Moving diagonally

The up and down commands explained earlier are actually part of the family of four diagonal motion commands:

```
C-M-u treesitedit-backward-up
C-M-d treesitedit-forward-down
C-M-p treesitedit-backward-down
C-M-n treesitedit-forward-up
```

These commands will always move up or down a level, and they may also move horizontally if needed.

The naming and behavior of these commands follow [Paredit](https://paredit.org) as closely as possible, with the big
difference of having to track the current node and accommodate moves that change it without moving point.

[The Animated Guide to Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html) is a great illustration
of the concept in the LISP original.

### Marking

Marking design is simple and inspired by [meow](https://github.com/meow-edit/meow) and
[expand-region](https://github.com/magnars/expand-region.el). `C-M-@` or `C-M-SPC` marks the node around point.
Subsequently pressing this key extends active region using the forward motion. To extend it backward, use `C-x C-x` to
`exchange-point-and-mark`.

### Killing

Killing with `C-M-k` is similar to marking with `C-M-SPC` and then killing the region.

## Alternatives

Try [mickeynp/combobulate](https://github.com/mickeynp/combobulate) for more features. The author has not used it since
it currently is lacking Go support.
