Sunroof
=======

Sunroof is a Haskell-hosted Domain Specific Language (DSL) for generating JavaScript.
Sunroof is build on top of the `JS`-monad, which, like the Haskell `IO`-monad, allows 
access to external resources, but specifically JavaScript
resources. As such, Sunroof is primarily a feature-rich foreign
function API to the browser's JavaScript engine, and all the browser-specific
functionality, including HTML-based rendering, event handling, and 
drawing to the HTML5 canvas. 

It uses monadic reification, to reify a deep embedding of the `JS`-monad,
and from this embedding it generates JavaScript.
The Sunroof DSL has the feel of native Haskell, with a simple
Haskell-based type schema to guide the Sunroof programmer.
Furthermore, because it generates code,
Sunroof can offer Haskell-style concurrency patterns, like `MVar`s and `Chan`nels.
In combination with a web services package like [`kansas-comet`][HackageKansasComet],
the Sunroof compiler offers a robust platform to build interactive web applications,
giving the ability to interleave Haskell and JavaScript computations
with each other as needed ([`sunroof-server`][HackageSunroofServer]).

Further Information
-------------------

Further information on Sunroof can be found in different places:

 *  [Project Homepage](http://www.ittc.ku.edu/csdl/fpg/software/sunroof.html)
 *  [Tutorial](https://github.com/ku-fpg/sunroof-compiler/wiki/Tutorial)
 *  [Examples](https://github.com/ku-fpg/sunroof-compiler/wiki/Examples)
 *  [Project Wiki & Development Resources](https://github.com/ku-fpg/sunroof-compiler/wiki)
 *  Hackage
     + [sunroof-compiler](http://hackage.haskell.org/package/sunroof-compiler)
     + [sunroof-server][HackageSunroofServer]
     + [sunroof-examples](http://hackage.haskell.org/package/sunroof-examples)
 *  Github
     + [sunroof-compiler](https://github.com/ku-fpg/sunroof-compiler)
     + [sunroof-server](https://github.com/ku-fpg/sunroof-server)
     + [sunroof-examples](https://github.com/ku-fpg/sunroof-examples)




[HackageKansasComet]: http://hackage.haskell.org/package/kansas-comet
[HackageSunroofServer]: http://hackage.haskell.org/package/sunroor-server

