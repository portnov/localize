localize README
===============

This package is more or less fully functional translation framework,
based on [haskell-gettext][1] package. The key features are:

* Convinient functions for translation.
* Full support of gettext features, such as message contexts and plural forms.
* It is possible to detect language to use by process locale. But it is not
  necessary, you for example may want to use `Accept-Language` HTTP header
  instead.
* Easy integration with custom monadic stacks by implementing instance of
  `Localized` type class.
* There are two simple implementations of `Localized` type class, for IO and
  StateT-based monad transformer. Being mostly examples, these implementations
  can be useful in relatively simple applications.
* Utility functions to locate catalog files by specified rules. Example rules
  for locating catalog files under source directory and for locating them
  under usual Linux locations are provided.
* Integration with [text-format-heavy][2] package. It may be more convinient
  than use of some other formatting libraries (`printf`, for example),
  since it supports named variable placeholders, much easier for translators
  to understand.

Please refer to Haddock documentation and to examples in `examples/` directory.

[1]: https://hackage.haskell.org/package/haskell-gettext
[2]: https://hackage.haskell.org/package/text-format-heavy

