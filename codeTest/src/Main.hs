module Main where

import Control.Applicative
import Faceted.Pure
import Faceted.FIO as FIO
import Faceted.FHandle

import Faceted.Internal as Internal


main :: IO()
main = do  
  secureRunFoo
  secureRunFoo2
  secureRunFoo3
  
-- the most basic use; no facets, just using the environment -----------------------------

secureRunFoo :: IO ()
secureRunFoo = secureRunFIO fioFoo
    
fioFoo :: FIO()      
fioFoo = FIO foo
    
foo :: PC -> IO()
foo branches = putStrLn "1: The gnomes have learned a new way to say, 'Hooray!'"
 
-- a bit more advanced ------------------------------------------
 
secureRunFoo2 :: IO()
secureRunFoo2 = secureRunFIO fioFoo2

fioFoo2 :: FIO()
fioFoo2 = FIO foo2
   
foo2 :: PC -> IO()
foo2 branches = putStrLn $ "2: " ++ show out
  where x = Faceted "A" (Raw 42) (Raw 0)
        y = Faceted "B" (Raw  2) (Raw 0)
        z = compact $ (>) <$> x <*> y
        out = Faceted "C" (Raw rules) (Raw drools) <*> z
  
rules :: (Show a) => a -> String
rules x = show x ++ " rules!"

drools :: (Show a) => a -> String
drools x = show x ++ " drools!"
  
-- I think Internal.project should be this; so as we unpack a faceted to
-- get whatever value the observer can see, we also get what must and must not
-- be visible in order to get the result.
-- The signature can be read like, "project a view through a faceted whatever"
project2 :: View -> Faceted a -> (PC, a)
project2 ____ (Raw v) = ([], v)
project2 view (Faceted k prv pub)
  | k `elem` view = (Public  k : accumPrv, vPrv)
  | otherwise     = (Private k : accumPub, vPub)
  where (accumPrv, vPrv) = project2 view prv
        (accumPub, vPub) = project2 view pub
  
-- If the public and private facets are the same, we can collapse them 
compact :: (Eq a) => Faceted a -> Faceted a
compact (Raw v) = Raw v
compact (Faceted lab prv pub)
    | prv == pub = compact pub
    | otherwise  = Faceted lab (compact prv) (compact pub)
  
-- Build up paths ---------------------------------------------------

secureRunFoo3 :: IO()
secureRunFoo3 = secureRunFIO $ FIO foo3

{-
  b = <correctUser ? 1234567 : 42>
  isRich = b > 999999
  if (isRich)
    out = 'Youre a millionaire!'
  else
    out = 'Youre not a millionaire!'
  print out
-}
foo3 :: PC -> IO()
foo3 activeLabels = putStrLn $ "3: " ++ project (pcToView activeLabels) out
  where activeLabels2 = activeLabels ++ [Private "correctUser"]
        b = Faceted "correctUser" (Raw 12345678) (Raw 42)
        isRich = (>) <$> b <*> Raw 999999
        (branches, isRichUnpacked) = project2 (pcToView activeLabels2) isRich
        out = if isRichUnpacked then pathOfTheTrue  (activeLabels2 ++ branches) 
                                else pathOfTheFalse (activeLabels2 ++ branches) 

-- May (s)he live in virtuosity  
pathOfTheTrue :: PC -> Faceted String
pathOfTheTrue activeLabels = makeMultifaceted activeLabels "It's true, all true!" "It's a secret to everybody"
 
-- May (s)he perish in ignominy  
pathOfTheFalse :: PC -> Faceted String
pathOfTheFalse activeLabels = makeMultifaceted activeLabels "Lies, all lies!" "It's a secret to everybody"
  
makeMultifaceted :: PC -> a -> a-> Faceted a
makeMultifaceted pc seekrit placebo = pcF pc (Raw seekrit) (Raw placebo)
  
pcToView :: PC -> View
pcToView [         ] = [ ]
pcToView [Private k] = [k]
pcToView [Public  k] = [k]
pcToView (branch:xs) = case branch of
  Private k -> k : pcToView xs
  Public  k -> k : pcToView xs

foo4 :: PC -> Faceted String 
foo4 activeLabels = do
  let d = Faceted "Don" (Raw 222) (Raw 11)
      e = Raw "merrrp"
      condF = (>) <$> Raw 50 <*> d
      thenF = Raw "Pish"
      elseF = Raw "Posh"
  facetedIf condF thenF elseF e
  
facetedIf :: Faceted a -> Faceted b -> Faceted b -> Faceted b -> Faceted b
facetedIf condFacet thenFacet elseFacet placebo = mergeBrnch
  where trueBranch = commuteFacets condFacet thenFacet  placebo
        falsBranch = commuteFacets condFacet placebo    thenFacet
        mergeBrnch = commuteFacets condFacet trueBranch falsBranch
  
commuteFacets :: Faceted a -> Faceted b -> Faceted b -> Faceted b
commuteFacets (Faceted layble _ _) = Faceted layble
  
  
  
  
  
  
  
  