module Data.JSON.ToString

import Data.JSON.Value
import Data.Scientific

-- TODO: check if this is working correctly!
private
isControlChar : Char -> Bool
isControlChar c =
  or [ cast c >= (0*1) && cast c <= (15*1 + 1*16)
     , cast c == (15*1 + 7*16)
     , cast c == (5*1 + 8*16)
     ]
-- TODO: this nice escaped version doesn't parse unfortunately
--  or [ c >= '\u0000' && c <= '\u001F'
--     , c == '\u007F'
--     , c == '\u0085'
--     ]

-- TODO: check if this is working correctly!
private
escapeUnicodeChar : Char -> List Char
escapeUnicodeChar c = ['\\', 'u'] ++ map fst [hex3, hex2, hex1, hex0] where
  f : Int -> (Char, Int)
  f x = (toChar (c), r) where
    c : Int
    c = x `mod` 16
    r : Int
    r = x `div` 16
    toChar : Int -> Char
    toChar x = cast $ if x < 10
                         then x + cast '0'
                         else x + cast 'A'
  hex0 : (Char,Int)
  hex0 = f $ cast c
  hex1 : (Char,Int)
  hex1 = f $ snd hex0
  hex2 : (Char,Int)
  hex2 = f $ snd hex1
  hex3 : (Char,Int)
  hex3 = f $ snd hex2

private
escapeControlChar : Char -> List Char
escapeControlChar c = if isControlChar c
                         then escapeUnicodeChar c
                         else [c]

private
escapeChars : List Char -> List Char
escapeChars [] = []
escapeChars (c :: cs) = c' ++ escapeChars cs where
  prependBackslash : Char -> List Char
  prependBackslash c = ['\\', c]
  c' : List Char
  c' = case c of
            '"' => prependBackslash '"'
            '\\' => prependBackslash '\\'
            '\b' => prependBackslash 'b'
            '\f' => prependBackslash 'f'
            '\n' => prependBackslash 'n'
            '\r' => prependBackslash 'r'
            _ => escapeControlChar c

public export
toString : JSONValue -> String
toString JSONNull = "null"
toString (JSONBool x) = case x of
                             True => "true"
                             False => "false"
toString (JSONNumber x) = prettyShowScientific x
toString (JSONString x) = pack $ escapeChars $ unpack $ x where
toString (JSONObject xs) = "{" ++ pairsToString xs ++ "}" where
  pairsToString : List (String, JSONValue) -> String
  pairsToString [] = ""
  pairsToString ((name, value) :: []) = toString (JSONString name) ++ ":" ++ toString value
  pairsToString (pair :: pairs) = pairsToString [pair] ++ "," ++ pairsToString pairs
toString (JSONArray xs) = "[" ++ valuesToString xs ++ "]" where
  valuesToString : List JSONValue -> String
  valuesToString [] = ""
  valuesToString (x :: []) = toString x
  valuesToString (x :: xs) = valuesToString [x] ++ "," ++ valuesToString xs
