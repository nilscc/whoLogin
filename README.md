whoLogin v2.0
===

Completly new version of the old whoLogin. New features are:

- GTK Graphical User Interface and a simple CLI modus
- Easy configuration
- Skipping Arty5 site (optional)

Screenshot: [whologin 2.0](http://n-sch.de/whologin-20.png)


Install
===

To install, clone this repo:

    git clone git://github.com/mcmaniac/whoLogin.git

Change into the directory:

    cd whoLogin

And run cabal:

    cabal install

This will install your binary in "$HOME/.cabal/bin" (on linux) or "C:\Users\...\AppData\Roaming\cabal\bin" (on Windows).

If you dont have cabal and/or haskell installed, make sure to install the [Haskell Platform](http://hackage.haskell.org/platform/) or use the packages of your distribution (you'll need ghc and cabal-install). You will also need the 32 bit version of the [GTK+ libraries](http://www.gtk.org/download.html) in your PATH.

Have fun!