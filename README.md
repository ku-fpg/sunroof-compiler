Sunroof
=======

Sunroof is a Monadic Javascript Compiler. The JS monad is an analog to the IO or ST monad,
but is executed on a Javascript engine.

Architecture
------------

    Your Application Here
    -----
    Sunroof
    -----
    Kansas Comet
    ------
    Scotty
    -----
    Warp
    -----

Installation
------------

To use sunroof you should checkout latest version of the following packages from
GitHub:

 * _Boolean_: https://github.com/ku-fpg/Boolean - This is not the original repository.
      We have some custom additions.
 * _kansas-comet_: https://github.com/ku-fpg/kansas-comet
 * _scotty_: https://github.com/ku-fpg/scotty

Then you can install the package with cabal:

    cabal install ./Boolean ./kansas-comet ./scotty/wai-middleware-static ./scotty ./sunroof

Assuming that all repositories were checked out into the current directory.
You may use scotty from hackage if you wish to, but it is not recommended:

    cabal install ./Boolean ./kansas-comet ./scotty/wai-middleware-static ./sunroof


