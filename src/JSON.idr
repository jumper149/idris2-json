module JSON

interface JSON a where
  encode : a -> String
  decode : String -> Maybe a
