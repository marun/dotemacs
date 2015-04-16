# dot-emacs
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

I'm looking forward to minimizing cruft so that I can evolve my
configuration more freely.
