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

### Aquamacs

You will need the "aquamacs24" branch of the Aquamacs source.  I'll
assume you already know build software on a Mac, and have the
appropriate tools to do so (Xcode, etc).

```bash
$ git clone --depth 3 git://github.com/davidswelt/aquamacs-emacs.git
... lot of stuff downloaded ...
$ cd aquamacs-emacs
$ git checkout aquamacs24
... more stuff downloaded ...
$ ./build-aquamacs
... build build build ...
$ mv nexstep/Aquamacs.app /Applications
```

You should now have a working copy of Aquamacs (using Emacs 24).
Start her up and make sure she works.  This will also create the
default initialization and customization files, which we'll use later.

For details see the
[Aquamacs Developer](http://aquamacs.org/development.shtml) page or
the
[Building Aquamacs](http://www.emacswiki.org/cgi-bin/wiki/BuildingAquamacs)
on EmacsWiki

### Aquastart

Now install the starter kik:

```shell
$ cd ~/Library/Preferences/Aquamacs Emacs/
$ git clone http://github.com/nedludd/aquastart.git
```

Add the following to `~/Library/Preferences/Aquamacs
Emacs/Preferences.el`:

```lisp
;; Aquastart -- Aquamacs Emacs Starter Kit
;; http://github.com/nedudd/aquastart.git

;; Set the base directory for Aquastart
(setq aquastart-dir (concat (file-name-directory
                    (or (buffer-file-name) load-file-name)) "aquastart/"))

;; set up our various directories to load
(add-to-list 'load-path aquastart-dir)
(require 'init)
```

Now restart Aquamacs.

## Configuration

Aquamacs follows OS/X convention and stores all its preferences and
customizations in `~/Library/Preferences/Aquamacs Emacs`.  I find this
a pain to type all the time, so I link it to a directory in my home
directory, the more Unix'y way (you don't have to do this if you don't
want to):

```
$ ln -s ~/Library/Preferences/Aquamacs\ Emacs ~/.aquamacs
```

(Let's call this base configuration directory "the aquamacs directory"
or "aquamacs-dir" from now on. The base directory for the starter kit
-- the one you cloned and added to "Preferences.el" we'll call "the
aquastart directory" or "aquastart-dir").

### Personalization

Anything you customize using the Emacs customize interface (.i.e. "M-x
customize-group") will be saved in "customization.el" in the aquamacs
directory.

If you want to add your own tweaks and configurations, here's the
convention (thanks to Phil Hagelburg):

In your aquastart-dir:

* Put your personalizations in a file called $USER.el (where "$USER"
of course is your real username).
* Any ".el" files in a directory called $USER will be loaded automatically.
* If you share your config between hosts, you can use a file called
  $HOST.el for host specific configuration (where $HOST is your hostname).

### Adding packages

Use the emacs "package.el" to install any packages/libraries you need.  If what
you want is not in the [Marmalade Repo](http://marmalade-repo.org)
(which the place to be these days if you're an Emacs package), you can
put it in your $USER directory, or better yet, upload it to Marmalade
so everyone can enjoy it.

#### Adding packages

Adding a package to Marmalade is quite easy. In fact there is a handy
tool included for doing just that.  If it's a single file package just
load it into a buffer and do:

```
M-x marmalade-upload-buffer
```

The file should have the standard package headers, although only
"Version: " is required.  It should have the following in the top
lines of the file:

```lisp
;; package-name.el -- package description

;; Author:  Author's name
;; Maintainer: Maintainer's name

;; Version: version number
;; Keywords: some descriptive tags, separated by comma's

;; Commentary:  documentation of the package
```

You do not need to be the author or maintainer of the package to
upload it to Marmalade.  You will need a login there though, so head
to http://marmalade-repo.org and get one.

## Acknowledgments

Aquastart draws inspiration -- and much stolen code -- from
Phil Hagelberg's [emacs-start-kit](https://github.com/technomancy/emacs-starter-kit)
(specifically version 2.x for Emacs 24), and
Bozhidar Batsov's [emacs-prelude](https://github.com/bbatsov/emacs-prelude).

The idea for an Aquamacs starter kit, and an initial attempt, was
forked from
Evangineer's [aquamacs-emacs-starter-kit](https://github.com/evangineer/aquamacs-emacs-starter-kit)
which was forked from
Walter McGinnis's [aquamacs-emacs-starter-kit](http://github.com/walter/aquamacs-emacs-starter-kit).


