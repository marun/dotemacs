# dotemacs
A minimal Emacs configuration for Python and Go development.

After 14+ years of accumulating cruft in my .emacs file, I finally hit
reset.  Changes include:

 - No more manually-curated 3rd-party dependencies.
   [cask](http://cask.readthedocs.org/) +
   [pallet](https://github.com/rdallasgray/pallet) are the future.

 - No more pymacs + ropemacs.  [elpy](http://elpy.readthedocs.org/) is
   better documented and can still use rope where needed.  It has some
   gaps, but I think I can tolerate them while I work on fixes.

 - No more custom-set-*.  All important variables are explicitly set
   with comments where appropriate.

 - Shiny new tools like [helm](https://github.com/emacs-helm/helm),
   [projectile](https://github.com/bbatsov/projectile),
   [magit](http://magit.vc/).

 - Navigation niceties like
   [AceJump](http://www.emacswiki.org/emacs/AceJump) and [Key
   Chord](http://www.emacswiki.org/emacs/KeyChord).

I'm looking forward to minimizing cruft so that I can evolve my
configuration more freely.

Some great resources for Emacs inspiration:

- <http://emacsredux.com/>
- <http://tuhdo.github.io/>
- <http://sachachua.com/blog/category/geek/emacs/>
