# Clojure Emacs Hacks

A collection of Emacs hacks and tweaks for Clojure development.

## Clojure Mode Extensions

`clojure-mode-ext.el` is a collection of Clojure development
extensions on top of the clojure-mode.

Provides `cljx/slime-compile-file` - which is a convenient way to
`(remove-ns ..)` the current namespace and recompile.

Provides `cljx/slime-edit-definition` which can fallback to using TAGS
to find Java interop code within .java files.

Adds some bindings in the slime and clojure-mode keymaps.

## Clojure Mode Slime

`clojure-mode-slime.el` restores slime integration in clojure-mode
2.0.

## Clojuredocs

`clojuredocs.el` provides a simple interface to lookup Clojure
documentation from the excellent
[clojuredocs.org](http://clojuredocs.org) web site, without leaving
the comfort of Emacs - courtesy of [w3m](http://w3m.sourceforge.net/)
and [emacs-w3m](http://www.emacswiki.org/emacs/emacs-w3m).

## Installation

Just add the following to your `~/.emacs` or `~/.emacs.d/init.el`
file:

```lisp
(require 'clojure-mode-ext)
(require 'clojure-mode-slime)
(require 'clojuredocs)
```

and, optionally add:

```lisp
(cljx/basic-init)
```

which adds some useful defaults for indenting with spaces only and
ensuring files are saved with a final newline.
