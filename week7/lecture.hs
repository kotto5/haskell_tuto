

isAdult :: Maybe Int -> Bool
isAdult m = case m of
    Just age -> age >= 18
    Nothing -> False

type Age = Int

isAdult' :: Maybe Age -> Bool
isAdult' m = case m of
    Just age -> age >= 18
    Nothing -> False

