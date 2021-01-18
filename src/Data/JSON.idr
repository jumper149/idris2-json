module Data.JSON

import public Data.JSON.Value
import Data.JSON.FromString
import Data.JSON.ToString

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
