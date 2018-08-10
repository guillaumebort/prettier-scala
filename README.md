# Scala implementation of Philip Wadler's "Prettier Printer"

See http://homepages.inf.ed.ac.uk/wadler/papers/prettier/prettier.pdf

However the combinators given by Philip Wadler are not expressive enough to allow the final layout we need for most languages. For example, in Scala literal new lines in multiline strings & comments must be preserved.

To allow to force the layout, we provide a `NoFlat` combinator that prevent any flatten. To reflect that we change the signature of flatten to indicate that some doc cannot be flattened. In this case the group combinator does not have any effect and acts as identity.

:point_right: This is a toy implementation. If you are looking for a real and optimized Scala implementation you should have a look at https://github.com/typelevel/paiges.