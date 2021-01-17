module JSON

public export
data JSONValue : Type where
  JSONNull : JSONValue
  JSONBool : Bool -> JSONValue
  JSONNumber : Integer -> JSONValue -- TODO: use scientific number
  JSONString : String -> JSONValue
  JSONObject : List (String, JSONValue) -> JSONValue
  JSONArray : List JSONValue -> JSONValue

public export
Eq JSONValue where
  JSONNull == JSONNull = True
  (JSONBool x) == (JSONBool y) = x == y
  (JSONNumber x) == (JSONNumber y) = x == y
  (JSONString x) == (JSONString y) = x == y
  (JSONObject xs) == (JSONObject ys) = xs == ys
  (JSONArray xs) == (JSONArray ys) = xs == ys
  _ == _ = False

public export
Show JSONValue where
  show JSONNull = "JSONNull"
  show (JSONBool x) = "JSONBool " ++ show x
  show (JSONNumber x) = "JSONNumber " ++ show x -- TODO: use scientific number
  show (JSONString x) = "JSONString " ++ show x
  show (JSONObject xs) = "JSONObject " ++ show xs
  show (JSONArray xs) = "JSONArray " ++ show xs

private
escapeControlChar : Char -> List Char

private
escapeChars : List Char -> List Char -- TODO: remove all other control characters
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
toString (JSONNumber x) = show x -- TODO: use scientific number
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

public export
fromString : String -> JSONValue -- TODO

public export
interface ToJSON a where
  toJSON : a -> JSONValue

encode : ToJSON a => a -> String
encode = toString . toJSON

public export
interface FromJSON a where
  fromJSON : JSONValue -> Maybe a

decode : FromJSON a => String -> Maybe a
decode = fromJSON . fromString
