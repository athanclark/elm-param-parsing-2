module Query exposing (..)

{-|
This library works with URI query strings; a string that's
either empty, or, beginning with `?` (question mark), is followed by
one or more strings, each of which separated by a `&` (ampersand).
These strings may or may not be split between a `=` (equals sign).

> In this library, we consider only the **first** `=` as the split,
> and all further occurances remain intact in the resulting `Just v` value.

If there is at least one `=` in the string, then the resulting split
will be `(k, Just v)`. Likewise, if there is no `=`, then the
result will just be `(k, Nothing)`. Critically, if the `=` is included,
but there is no string before the next `&` (i.e. `...&foo=&...`), then
the result will be `("foo", Just "")`. Empty strings (i.e. `...&&...`)
are represented as `("", Nothing)`.

We then have a list of pairs of strings with a nullable right value.
This is a precise type denoting the query string, opposed to a `Dict` -
it retains both the ordering information of the query strings, and their
possible value. A more general type might be `List String`, so the resulting
type would be a list of lists `List (List String)`, but this library is
intended to assume a more practical use case.

## Laws:

### Isomorphism

forall query strings `x:String` (i.e. `x-www-uriencode` formatted strings, separated
by `=` and `&`, beginning with a `?`):

```
printQuery (parseQuery x) == x
```

and likewise, forall query string sets `xs:[(String, Maybe String)]`:

```
parseQuery (printQuery xs) == xs
```

> If you can think of others, please let me know!

@docs parseQuery, printQuery
-}

import String
import Http


{-|
Parse everything after the path and before the fragment,
specifically `/path/to/resource.foo...#fragment`. This
function's behaviour is unspecified if you give this whole
string, and not only the portion marked as `...`, usually
in the form of `?foo=bar&baz&qux=2` *including* the initial `?`.
Note that this also decodes the `x-www-uriencode` data as well
with `Http.uriDecode`.
-}
parseQuery : String -> List (String, Maybe String)
parseQuery q =
  case String.uncons q of
    Nothing -> []
    Just (questionMark, toParse) ->
      if questionMark /= '?'
      then []
      else
        let getKV : String -> (String, Maybe String)
            getKV s =
              case String.split "=" s of
                []        -> ("", Nothing)
                [k]       -> ( Http.uriDecode k
                             , Nothing
                             )
                (k :: vs) -> ( Http.uriDecode k
                             , Just <| String.concat -- FIXME
                                    <| List.intersperse "="
                                    <| List.map Http.uriDecode vs
                             )
        in  List.map getKV <| String.split "&" toParse

{-|
Prints out the query Dict such that a `?` is placed
only if the Dict is _non-empty_. Also uses `Http.uriEncode` from
[evancz's http package](https://package.elm-lang.org/packages/evancz/elm-http),
and does _not_ place a `&` if the value is `Nothing`.
-}
printQuery : List (String, Maybe String) -> String
printQuery xs =
  case xs of
    [] -> ""
    (kMv :: kMvs) ->
      let fromKV : (String, Maybe String) -> String
          fromKV (k,mV) =
            case mV of
              Nothing -> Http.uriEncode k
              Just v  -> Http.uriEncode k ++ "=" ++ Http.uriEncode v
          go = String.concat << List.intersperse "&" << List.map fromKV
      in  "?" ++ go (kMv :: kMvs)
