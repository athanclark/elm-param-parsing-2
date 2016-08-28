module Main exposing (..)

import Query exposing (parseQuery, printQuery)
import String
import Http
import Fuzz exposing (Fuzzer, string, list, tuple, maybe)
import Test exposing (Test, describe, fuzz)
import Expect
import Test.Runner.Html


queryString : Fuzzer String
queryString =
  Fuzz.map mkQueryString string


mkQueryString : String -> String
mkQueryString s =
  case String.uncons s of
    Nothing -> "?"
    Just (q,xs) ->
      if q /= '?'
      then "?" ++ Http.uriEncode s -- not a query string
      else let ys = List.map (String.split "=") <| String.split "&" xs
           in  String.concat <| List.map (String.concat << List.map Http.uriEncode) ys


queryParams : Fuzzer (List (String, Maybe String))
queryParams = list <| tuple (string, maybe string)


suite : Test
suite =
  describe "Parse / Print Pseudo-Iso"
    [ fuzz queryString
        "(print . parse) == identity"
        <| \s  -> printQuery (parseQuery s) `Expect.equal` s
    , fuzz queryParams
        "Printing a set of query paramaters then parsing is is the identity"
        <| \xs -> parseQuery (printQuery xs) `Expect.equal` xs
    ]


main : Program Never
main = Test.Runner.Html.run suite
