-- map using foldl.
map2 f = foldl (\xs x -> xs ++  [f x]) []

