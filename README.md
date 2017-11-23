# elm-decode-pipeline-strict

A strict version of [elm-decode-pipeline](http://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest).

## What do you mean by strict?

Decoding fails if there are attributes in the Json objects that are not referenced in the decoder.
