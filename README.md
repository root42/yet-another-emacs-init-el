yet-another-emacs-init-el
=========================

This is a nice, rather small init.el for Emacs24.4 and up.

It will install a selected range of packages upon first startup. It will ask you before installing each package.

# Installation

Installation is as simple as copying init.el to your ~/.emacs.d directory. I recommend however to use a symlink for easy updates:

    cd
    git clone https://github.com/root42/yet-another-emacs-init-el
    cd ~/.emacs.d
    ln -s ../yet-another-emacs-init-el/init.el .
    cp ../yet-another-emacs-init-el/custom.el .

## Installing to a custom place

You can also put this package at some arbitrary place and tell emacs to use the init.el at this custom place:

    cd
    git clone https://github.com/root42/yet-another-emacs-init-el
    emacs -q --load ~/yet-another-emacs-init-el

Customizations will be saved in custom.el in the same directory.

# Installed Packages

The following packages will be auto-installed upon first startup:

* auctex
* cider
* clojure-mode
* company
* csv-mode
* ecb
* exec-path-from-shell
* flycheck
* ggtags
* helm
* jedi
* js2-mode
* magit
* paredit
* reftex
* skewer-mode
* tabbar

# Shortcuts

This init.el defines a number of useful shortcuts.

Shortcut  | Description
----------|-------------
f1      | Runs cider-refresh which reloads all clojure classes in the current cider REPL.
f2      | Runs dabbrev-expand, which tries to cleverly complete the symbol at point.
f3      | Switch between header and implementation for C/C++ programs.
f4      | Toggles the last two used buffers.
f5,f6 | If tabbar is enabled (tabbar-mode), navigates back/forward through tabs.
C-tab,C-S-tab | Same as above.
f7              | Toggle ispell dictionaries (german/english).
f8              | Kill current buffer.
f9              | Run compile.
C-f9            | Compile current module.
M-?             | Run grep
M-n             | Go to next error in compilation buffer.
M-S-n           | Go to first error in compilation buffer.
M->, M-<        | Go to next/previous Emacs frame.
M-/             | Run autocompletion using company-complete.
C-x o, C-x C-o  | Go forward/backward through Emacs windows.
M-.             | Find tag using ggtags.
M-]             | Find references using ggtags.
