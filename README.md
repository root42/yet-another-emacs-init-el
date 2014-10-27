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

# Installed Packages

The following packages will be auto-installed upon first startup:

   cider
   clojure-mode
   exec-path-from-shell
   flycheck
   neotree
   js2-mode
   paredit
   helm
   skewer-mode
   tabbar
   magit
   auctex
   reftex

# Shortcuts

This init.el defines a number of useful shortcuts.