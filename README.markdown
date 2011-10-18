
# Aquastart - An Aquamacs Emacs Starter Kit

This kit tries to provide a nice base for Aquamacs.  It is intended
mostly for beginner to intermediate users, but hopefully offers a
framework to be built up for any user.

Aquamacs is already heavily customized, this just offers some nice
defaults.

This starter kit requires the use of the Emacs 24 branch of Aquamacs.
See the Installation section for more details.

## Guiding concepts

* Where possible use the original Aquamacs configuration files
  (customization.el and Preferences.el) so as not to duplicate effort
  or add complexity.

* Use Emacs 24 built-in package.el where possible to install
additional libraries.

* Focus mostly on tools for web developers and system administrators.

* In contrast to Aquamacs defaults, use new windows (frames in Emacs
speak) sparingly.

* Use Emacs 24 new themes interface, as opposed to the old color-theme libraries.

## Installation

### Aquamacs itself

You will need the "aquamacs24" branch of the Aquamacs source.  I'll
assume you already know build software on a Mac, and have the
appropriate tools to do so (Xcode, etc).

```Shell
$ git clone --depth 3 git://github.com/davidswelt/aquamacs-emacs.git
... lot of stuff downloaded ...
$ cd aquamacs-emacs
$ git checkout aquamacs24
... more stuff downloaded ...
$ ./build-aquamacs
... build build build ...
$ mv nexstep/Aquamacs.app /Applications
````

You should now have a working copy of Aquamacs (using Emacs 24).
Start her up and make sure she works.  This will also create the
default initialization and customization files, which we'll use later.

For details see the
[Aquamacs Developer page](http://aquamacs.org/development.shtml) or
the
[Building Aquamacs](http://www.emacswiki.org/cgi-bin/wiki/BuildingAquamacs)
on EmacsWiki

### Aquastart

Now install the starter kik:

```Shell
$ cd ~/Library/Preferences/Aquamacs Emacs/
$ git clone http://github.com/nedludd/aquastart.git
```

Add the following to `~/Library/Preferences/Aquamacs
Emacs/Preferences.el`:

```Lisp
;; Aquastart -- Aquamacs Emacs Starter Kit
;; http://github.com/nedudd/aquastart.git

;; Set the base directory for Aquastart
(setq aquastart-dir (concat (file-name-directory
                    (or (buffer-file-name) load-file-name)) "/aquastart"))

;; set up our various directories to load
(add-to-list 'load-path aquastart-dir)
(require 'init)
````

Now restart Aquamacs.


## Acknowledgments

Aquastart draws inspiration -- and much stolen code -- from
[Phil Hagelberg's emacs-start-kit](https://github.com/technomancy/emacs-starter-kit)
(specifically version 2.x for Emacs 24), and
[Bozhidar Batsov's emacs-prelude](https://github.com/bbatsov/emacs-prelude).

The idea for an Aquamacs starter kit, and an initial attempt, was
forked from
[Evangineer's aquamacs-emacs-starter-kit](https://github.com/evangineer/aquamacs-emacs-starter-kit)
which was forked from
[Walter McGinnis's aquamacs-emacs-starter-kit](http://github.com/walter/aquamacs-emacs-starter-kit).


