dotemacs
========

Just my .emacs files, for backup purposes.  I clone this repo to `~/.dotemacs`,
and then symbolically link
```bash
ln -sf ~/.dotemacs/.emacs ~/.emacs
ln -sf ~/.dotemacs/.emacs.elc ~/.emacs.elc
```


## Dependencies

`/usr/texbin`

`/usr/local/bin/aspell`


## Packages

```lisp
anaconda-mode     Code navigation, documentation lookup and completion for Python
auctex            Integrated environment for *TeX*
company           Modular text completion framework
company-anaconda  Anaconda backend for company-mode
markdown-mode     Emacs Major mode for Markdown-formatted text files
redo+             Redo/undo system for Emacs
yaml-mode         Major mode for editing YAML files
zotelo            Manage Zotero collections from emacs
```
