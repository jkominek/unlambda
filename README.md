Not fully tested, but should support Unlambda as described on Esolang:
https://esolangs.org/wiki/Unlambda

Additionally, I/O is on Unicode characters, and ~ is introduced as special
syntax to define a named function, and provide it from the module. For
instance, the program ~Ii will provide the identity function as 'I'.

Todo items:
* Make interactions work
  * #%top-interaction is insufficient; we require our special parser
* Test cases
* ???
