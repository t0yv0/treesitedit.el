# treesitedit.el

Minor mode for making editing motions better with treesitter.

## Usage

```emacs-lisp
(require 'treesitedit)
(treesitedit-mode)
```

### Motion

#### By Expression

Moving by forward and backward by expression is very similar to the
[standard](https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html) but uses treesitter to identify
the bounds of the current expression.

#### By Block

The original [Moving by Parens](https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-by-Parens.html) motion
is re-imagined to take advantage of treesitter structure. Instead of working with balanced `{` and `}` braces, the
motion considers "blocky" nodes such as `for {}` in Go as units to move over.

### Marking

`C-M-@` or `C-M-SPC` marks the node around point. Subsequently pressing this key extends active region using the forward
motion. To extend it backward, use `C-x C-x` to `exchange-point-and-mark`.

### Killing

Killing with `C-M-k` is similar to marking with `C-M-SPC` and then killing the region.

## References

- [mickeynp/combobulate](https://github.com/mickeynp/combobulate) for more features.
  The author has not used it since it currently is lacking Go support.

- [meow](https://github.com/meow-edit/meow) for very effective modal editing motions.

- [Paredit](https://paredit.org) is an inspiration for LISP editing motions.
  See also [The Animated Guide to Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html)

  Translating Paredit directly to Algol-style languages is challenging because POINT can correspond to more than one
  node in the syntax tree.
