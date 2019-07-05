{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module TH where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text      as P
import           Data.Char
import           Data.Generics
import           Data.Text                 (pack, unpack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (takeWhile)

addMaybesAndOpts :: Maybe String -> Dec -> Q Dec
addMaybesAndOpts modName dec =
  -- Apply @rename@ and @addMaybe@ everywhere in the
  -- declaration @dec@.
  --
  -- The SYB, @everywhere (mkT (f :: a -> a)) (x :: b)@
  -- applies @f@ to all data of type @a@ in @x@, and
  -- @everywhereM (mkM (f :: a -> m a) (x :: b)@ is
  -- similar, but applies @f@ everywhere in @x@ monadically.
  everywhere (mkT rename) <$>
  everywhereM (mkM addMaybe) dec
  where
    -- Add the "_opt" suffix to a name, if it's from
    -- the given module.
    rename :: Name -> Name
    rename n = if nameModule n == modName
        then mkName $ nameBase n ++ "_opt"
        else n

    -- Wrap the type of a record field in @Maybe@.
    addMaybe :: (Name, Strict, Type) -> Q (Name, Strict, Type)
    addMaybe (n, s, ty) = do
      ty' <- [t| Maybe $(return ty) |]
      return (n,s,ty')

mkOptional :: Name -> Q Dec
mkOptional n = do
    TyConI dec <- reify n
    let DataD d _ _ _ _ _ = dec
    t <- reify ''Int
    liftIO $ print t
    addMaybesAndOpts (nameModule n) dec

unionRecord = QuasiQuoter
  { quoteDec = unionD
  , quoteExp = undefined
  , quotePat = undefined
  , quoteType = undefined
  }

unionD input =  do
  let s = either error id $ parseOnly unionRecordParser $ pack input
  liftIO $ print s
  ss <- forM (snd s) $ \r -> do
    Just sub <- lookupTypeName r
    TyConI dec@(DataD ctx name bndrs kind ((RecC rname rtys):_) _) <- reify sub
    pure (ctx, bndrs, kind, rtys)
  name' <- newName (fst s)
  rname' <- newName (fst s)
  -- pure [DataD cxt name' bndrs kind [(RecC rname' rtys)] [derivs']]
  liftIO $ print $ ss
  pure [DataD (join $ fmap fst4 ss) name' (join $ fmap snd4 ss) Nothing [RecC rname' (join $ fmap fth4 ss)] []]

fst4 (x,_,_,_) = x
snd4 (_,x,_,_) = x
trd4 (_,_,x,_) = x
fth4 (_,_,_,x) = x

type RecordUnion = (String, [String])
unionRecordParser = do
  spaces
  n <- many1 safeN
  spaces
  char '='
  rs <- many $ do
    spaces
    r <- many1 safeN
    spaces
    pure r
  pure $ (n, rs)


-- extend = QuasiQuoter
--   { quoteDec = extendD
--   , quoteExp = undefined
--   , quotePat = undefined
--   , quoteType = undefined
--   }

-- extendD input = do
--   let s = either error id $ parseOnly parser $ pack input
--   liftIO $ print s
--   rtys' <- fields s $ \f -> do
--     case f of
--       Left sup -> do
--         Just sub <- lookupTypeName sup
--         TyConI dec@(DataD cxt name bndrs kind ((RecC rname rtys):_) _) <- reify sub
--         pure rtys
--   name' <- newName (name s)
--   rname' <- newName (name s)
--   dcons <- forM (derivs s) $ \d -> do
--     Just c <- lookupTypeName d
--     pure $ ConT c
--   let derivs' = DerivClause Nothing dcons
--   -- pure [DataD cxt name' bndrs kind [(RecC rname' rtys)] [derivs']]
--   pure [DataD [] name' [] Nothing]

-- parser = do
--   spaces
--   n <- many safe
--   spaces
--   char '{'
--   let spread = do
--         forM_ [0..2] $ \_ -> char '.'
--         many safe
--   fs <- many $ do
--     spaces
--     Left <$> spread
--   spaces
--   char '}'
--   spaces
--   char '('
--   derivs <- many $ do
--     deriv <- many safe
--     spaces
--     char ',' <|> char ')'
--     spaces
--     pure deriv
--   pure $ ExtendRecordSyntax n fs derivs
--   where
spaces = many space
safeN = letter <|> digit <|> choice (char <$> "_'")

-- data ExtendRecordSyntax = ExtendRecordSyntax
--   { name   :: String
--   , fields :: [Either String (String, String)]
--   , derivs :: [String]
--   } deriving (Show)
