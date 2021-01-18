module JSON.Value

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
