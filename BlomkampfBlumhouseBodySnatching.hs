module BlomkampfBlumhouseBodySnatching where

data Body a = Body a deriving Show

instance Functor Body where
  fmap f (Body a) = Body (f a)

data Local a = Local a deriving Show
data Foreign a = Foreign a deriving Show
data Entity a = Organic a | Android a deriving Show

invade :: Body (Local a) -> (Foreign b) -> Body (Local a, Foreign b)
invade a b = fmap (\x -> (x, b)) a

evict :: Body (Local a, Foreign b) -> Body (Foreign b)
evict = fmap snd

overtake :: Body (Local a) -> (Foreign b) -> Body (Foreign b)
overtake a b = evict $ invade a b

mutate :: Local (Entity (Body (Local a))) -> ((Body (Local a)) -> (Entity (Body (Local a)))) -> Foreign (Entity (Body (Local a)))
mutate (Local (Organic a)) f = Foreign (f a)
mutate (Local (Android a)) f = Foreign (f a)

getOut = invade (Body $ Local $ Organic ()) (Foreign $ Organic ())
upgrade = overtake (Body $ Local $ Organic ()) (Foreign $ Android ())
district9 = mutate (Local $ Organic $ Body $ Local $ Organic ()) (\b -> Organic b)
chappie = ((mutate (Local $ Organic $ Body $ Local $ Organic ()) (\b -> Android b)),
           (mutate (Local $ Android $ Body $ Local $ Android ()) (\b -> Android b)))

