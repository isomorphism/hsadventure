This is a modified version of a text adventure EDSL by Brent Yorgey. I do 
apologize to darcs users for dumping this into github instead of forking the 
original darcs repo, but... I've never used darcs, and I have a github account, 
so here we are.

Original version
-------
Find it here: <http://byorgey.wordpress.com/2011/05/27/bit-rotted-text-adventure-edsl-free-to-a-good-home/>

`master` branch:
-------

Minimal changes to make it easier to build.

- Includes an extra package, `category`, the original version of which can be
  found at <http://code.haskell.org/category/>. This was written by 
  [Twan van Laarhoven](http://twanvl.nl/index) and provides 
  [functional references](http://twanvl.nl/blog/haskell/References-Arrows-and-Categories)
  a.k.a. lenses used by the `adventure` package.

- Changes from original are mostly tweaking dependencies and slight changes to
  make the TH in `category` work with more recent versions (since I couldn't
  get older ones to install).

`refactoring` branch:
---------

More drastic changes I've made, mostly to depdencies: removing `MaybeT` in 
favor of more recent standard packages, and removing `category` in favor of
`fclabels`.

Any further changes will be made on the `refactoring` branch for the time being.


