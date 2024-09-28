# treesitedit.el

Minor mode for making editing motions better with treesitter.

## Usage

```emacs-lisp
(require 'treesitedit)
(treesitedit-mode)
```

### Motion

#### By Expression

Moving by forward and backward by expression rebinds `forward-sexp` and `backward-sexp` (see [Expressions with Balanced
Parentheses](https://www.gnu.org/software/emacs/manual/html_node/emacs/Expressions.html)) which are typically bound to
`C-M-f` and `C-M-b`. The treesitter-enabled motion is very similar but more precise. One intentional difference is that
it ignores hierarchies, blocks and parentheses. It behaves a bit more like `forward-word` than `forward-sexp`.

#### By Block

[Moving by Parens](https://www.gnu.org/software/emacs/manual/html_node/emacs/Moving-by-Parens.html) motions are
re-imagined to take advantage of treesitter structure, rebinding the standard motions:

```
C-M-p forward-list
C-M-n backward-list
C-M-u backward-up-list
C-M-d down-list
```

The interesting bit is that instead of working with balanced `{` and `}` braces, these motions consider "blocky" nodes
such as statements like `for {}` or parenthesized expressions `(1 + 2)` in Go as units.

Each language can define which nodes are considered "blocky" by customizing `treesitedit-block-nodes`. Some care must be
taken to ensure that these nodes are not overlapping. For example, expressions wrapped in `()` or `""` are guaranteed to
not overlap, but `expression_statement` may overlap `argument_list`. If overlapping nodes are marked as blocky, moving
over them is no longer symmetrical as the system may get confused as to which node is "current." It is not a huge
problem on occasion but is best avoided if possible.

When tweaking the language support, `treesit-explore-mode` is invaluable to understand the representation.

### Marking

`C-M-@` or `C-M-SPC` marks the node around point. Subsequently pressing this key extends active region using the
`forward-list` style motion. To extend it backward, use `C-x C-x` to `exchange-point-and-mark`.

There is some intentional divergence here from the standard implementation that is based on `forward-sexp` instead of
`forward-list`. This intentionally makes marking coarser-grained by default.

### Killing

Killing with `C-M-k` is similar to marking with `C-M-SPC` and then killing the region.

## References

- [mickeynp/combobulate](https://github.com/mickeynp/combobulate) for more features.
  The author has not used it since it currently is lacking Go support.

- [meow](https://github.com/meow-edit/meow) for very effective modal editing motions.

- [Paredit](https://paredit.org) is an inspiration for LISP editing motions.
  See also [The Animated Guide to Paredit](http://danmidwood.com/content/2014/11/21/animated-paredit.html)
