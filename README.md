[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

# temporary-persistent

temporary-persistent - easy way to switch temp buffers and keep them persistent.

When you open your temp buffer you don't need to save it manually,
it'll be saved automatically any time you kill this buffer or Emacs.

See also: [persistent-scratch](https://github.com/Fanael/persistent-scratch)

## Installation

Add MELPA (if not yet) to your `package-archives` list:

```lisp
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
```

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

### Save buffer keybinding

Thre is 3 cases Emacs save your temp buffer:

* `kill-buffer`
* `kill-emacs`
* Save buffer manually by prefered keybinding:

```lisp
(setq 'temporary-persistent-save-key "C-x C-s")
```

<kbd>C-x C-s</kbd> is used by default.

### Default submodes

You can enable some submodes for new temp buffers by default:

```lisp
(setq temporary-persistent-default-submodes (list 'linum-mode
                                                  'auto-fill-mode
                                                  'auto-complete-mode))
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

## License

Copyright © 2016 Kostafey <kostafey@gmail.com>

Distributed under the General Public License 3.0+
