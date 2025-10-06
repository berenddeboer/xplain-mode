# About

This is an Emacs mode to edit Xplain files. Xplain is a [Semantic Data
modeling language](https://www.jhterbekke.net/DataLanguage.html).

# Usage

Install using [straight.el](https://github.com/radian-software/straight.el):

```elisp
(use-package xplain-mode
  :straight (:host github :repo "berenddeboer/xplain-mode")
  :mode "\\.ddl$")
```

Or if already installed:

```elisp
(use-package xplain-mode
  :mode "\\.ddl$")
```
