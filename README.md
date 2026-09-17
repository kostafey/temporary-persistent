[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](LICENSE)
[![MELPA](https://melpa.org/packages/temporary-persistent-badge.svg)](https://melpa.org/#/temporary-persistent)

# temporary-persistent

temporary-persistent - easy way to switch temp buffers and keep them persistent.

When you open a temp buffer you don't need to save it manually,
it'll be saved automatically any time you kill this buffer or Emacs.

See also: [persistent-scratch](https://github.com/Fanael/persistent-scratch)

## Installation

Add [MELPA](https://github.com/melpa/melpa#usage) (if not yet) to your
`package-archives` list.

Then you can install temporary-persistent with the following command:

<kbd>M-x package-install [RET] temporary-persistent [RET]</kbd>

## Configuration

### Switch to temp buffer keybinding

The only usefull function you should bind to customize this package is
`temporary-persistent-switch-buffer`.
As for me, I'm rarely close my Emacs, so I dislike to waste handy 
<kbd>C-x C-c</kbd> keys. But you can use any bindings you like.

```lisp
(global-set-key (kbd "C-x C-c") 'temporary-persistent-switch-buffer)
```

Then, when you press <kbd>C-x C-c</kbd>, `*temp*` buffer will be created.
When you press <kbd>M-1 C-x C-c</kbd>, `*temp-1*` buffer will be created and so on.

There are 3 cases Emacs save your temp buffer:

* `kill-buffer`
* `kill-emacs`
* Save buffer manually via ordinary `save-buffer` function.

### Switch to temp buffer with `consult`

If [consult](https://github.com/minad/consult) is installed, there is one more
way to switch to a temp buffer: `temporary-persistent-consult-switch-buffer`.
It lists the live temp buffers via `consult-buffer`, showing a summary of the
buffer contents next to the buffer name instead of its file path:

* `markdown-mode` buffers: the text of the first level 1 heading (`#`),
* `org-mode` buffers: the `#+title:` value, or the text of the first level 1
  heading (`*`) if there is no `#+title:`,
* any other mode, or no such heading found: the first non-blank line.

The summary is part of the completion candidate, not of its annotation, so
what you type narrows the list by the buffer name and by the summary alike:
`weekly` finds `*temp-12*` when it starts with `#+title: Weekly plan`, and
`12` still finds it by name.  The `major-mode` is shown as the annotation and
is not matched against.

```lisp
(global-set-key (kbd "C-x C-t") 'temporary-persistent-consult-switch-buffer)
```

The completion source itself is available as
`temporary-persistent-consult-source`, so it can be added to the usual
`consult-buffer` list as well (narrowing key <kbd>t</kbd>):

```lisp
(add-to-list 'consult-buffer-sources 'temporary-persistent-consult-source t)
```

Note that temp buffers belong to the standard `Buffer` source as well, so they
are listed twice then.  To keep the annotated entries only, hide them from the
standard source (the regexp follows `temporary-persistent-buffer-name-template`):

```lisp
(add-to-list 'consult-buffer-filter "\\`\\*temp\\(-[0-9]+\\)?\\*\\'")
```

`temporary-persistent-consult-source` ignores `consult-buffer-filter`, so
`temporary-persistent-consult-switch-buffer` still lists all the temp buffers.

The columns can be adjusted with `temporary-persistent-consult-mode-width`,
`temporary-persistent-consult-summary-width`,
`temporary-persistent-consult-mode-face` and
`temporary-persistent-consult-summary-face`.

### Default major-mode

Set default `major-mode` for new temp buffers:

```lisp
(setq temporary-persistent-default-major-mode 'markdown-mode)
```

### Default submodes

You can enable some submodes for new temp buffers by default:

```lisp
(setq temporary-persistent-default-submodes (list 'auto-fill-mode))
```

### Directory to keep temporary buffers data

Change folder to keep temporary buffers data if you like:

```lisp
(setq temporary-persistent-store-folder "~/temp")
```

### Buffer name template

You can also change template for temporary buffer names:

```lisp
(setq temporary-persistent-buffer-name-template "temp")
```

## Requirements:

* [GNU Emacs](http://www.gnu.org/software/emacs/emacs.html) 24.
* [names](https://github.com/Malabarba/names).
* [dash.el](https://github.com/magnars/dash.el).
* [s.el](https://github.com/magnars/s.el).
* [consult](https://github.com/minad/consult) (optional, for
  `temporary-persistent-consult-switch-buffer`).

## License

Copyright © 2016-2026 Kostafey <kostafey@gmail.com>

Distributed under the General Public License 3.0+, see [LICENSE](LICENSE).
