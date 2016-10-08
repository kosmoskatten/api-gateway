module Char.Extra exposing
  ( isAlpha
  , isSpace
  , isFirstCharAlpha
  )

{-| Missing functions from the Char module -}

import Char exposing (toLower)
import List exposing (head)
import String exposing (left, toList)

{-| Check if the given character is alphabetic -}
isAlpha : Char -> Bool
isAlpha c =
  let c' = toLower c
  in c' >= 'a' && c' <= 'z'

{-| Check if the given character is a whitespace -}
isSpace : Char -> Bool
isSpace c = c == ' ' || c == '\n' || c == '\r' || c == '\t'

{-| Is the first character alphabetic? -}
isFirstCharAlpha : String -> Bool
isFirstCharAlpha str =
  case firstChar str of
    Just c  -> isAlpha c
    Nothing -> False

firstChar : String -> Maybe Char
firstChar = head << toList << left 1
