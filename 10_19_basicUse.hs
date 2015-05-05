import Control.Applicative

main :: IO()
main = do  
  putStrLn "Hello Person A, what's your name?"  
  nameA <- getLine  
  putStrLn $ "Hey " ++ nameA ++ ", what's your secret?"
  secretA <- getLine
  putStrLn "Hello Person B, what's your name?"
  nameB <- getLine
  putStrLn $ "Hey " ++ nameB ++ ", wanna hear " ++ nameA ++ "'s secret?"
  let placebo = "GiantCrabMonsterException: Null pointer reference to 404 file not found."
  putStrLn $ "Of course you do, it's: " ++ concealAndReveal nameA nameB secretA placebo
  
concealAndReveal :: Label -> Label -> a -> a -> a
concealAndReveal nameA nameB secretA placebo = revealed
  where concealed = concealSecret nameA secretA placebo
        revealed = revealSecret concealed nameB

concealSecret :: Label -> a -> a -> Faceted a
concealSecret multipass secret placebo = Faceted multipass (Raw secret) (Raw placebo)

revealSecret :: Faceted a -> Label -> a
revealSecret (Raw v) _ = v
revealSecret (Faceted l1 x y) l2 | l1 == l2  = revealSecret x l2
                                 | otherwise = revealSecret y l2

-- ----------------- to be replaced by faceted library ------------------------------------------
type Label = String

data Faceted a =
    Raw a
  | Faceted Label (Faceted a) (Faceted a)
  deriving (Show, Eq)

-- | Functor: For when the function is pure but the argument has facets.
instance Functor Faceted where
  fmap f (Raw v)              = Raw (f v)
  fmap f (Faceted k priv pub) = Faceted k (fmap f priv) (fmap f pub)

-- | Applicative: For when the function and argument both have facets.
instance Applicative Faceted where
  pure = Raw
  (Raw f) <*> x  =  fmap f x
  (Faceted k priv pub) <*> x  =  Faceted k (priv <*> x) (pub <*> x)

-- | Monad: Like applicative, but even more powerful. 'Faceted' the free monad
-- over the function 'Facets a = F Label a a'. 
instance Monad Faceted where
  return = Raw
  (Raw x)              >>= f  = f x
  (Faceted k priv pub) >>= f  = Faceted k (priv >>= f) (pub >>= f)

  
