{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module TH where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attoparsec.Text      as P
import           Data.Char
import           Data.Generics
import           Data.Maybe
import           Data.Text                 (pack, unpack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (takeWhile)

extendRecord = QuasiQuoter
  { quoteDec = extendD
  , quoteExp = undefined
  , quotePat = undefined
  , quoteType = undefined
  }

extendD :: String -> Q [Dec]
extendD input =  do
  let s = either error id $ parseOnly unionRecordParser $ pack input
  liftIO $ print s
  ss <- forM (elems s) $ \case
    SupType r -> do
      sub <- maybe (error $ "not in scope type " <> r) pure =<< lookupTypeName r
      TyConI dec@(DataD ctx name bndrs kind ((RecC _ rtys):_) _) <- reify sub
      pure (ctx, bndrs, rtys)
    Fields fs -> do
      rtys <- forM fs $ \f -> do
        let n = mkName (fst f)
        t <- maybe (error $ "not in scope type " <> (snd f)) pure =<< lookupTypeName (snd f)
        pure $ (n, (Bang NoSourceUnpackedness NoSourceStrictness), ConT t)
      pure ([], [], rtys)
  name' <- newName (name s)
  rname' <- newName (name s)
  dcons <- forM (derivs s) $ \d -> do
    c <- maybe (error $ "not in scope class " <> d) pure =<< lookupTypeName d
    pure $ ConT c
  let derivs' = DerivClause Nothing dcons
  -- pure [DataD cxt name' bndrs kind [(RecC rname' rtys)] [derivs']]
  liftIO $ print $ ss
  pure [DataD (join $ fmap fst3 ss) name' (join $ fmap snd3 ss) Nothing [RecC rname' (join $ fmap trd3 ss)] [derivs']]

fst3 (x,_,_) = x
snd3 (_,x,_) = x
trd3 (_,_,x) = x

data UnionRecord = UnionRecord
  { name   :: String
  , elems  :: [Element]
  , derivs :: [String]
  } deriving (Show, Eq)

data Element = SupType String | Fields [(String, String)]
  deriving (Show, Eq)

unionRecordParser = do
  spaces
  string "data"
  spaces
  n <- many1 safeN
  spaces
  char '='
  let pname = spaces *> many1 safeN <* spaces
  let f = do
        n <- pname
        string "::"
        t <- pname
        pure (n, t)
  let precord = do
        spaces *> char '{'
        fs <- f `sepBy` char ','
        spaces *> char '}'
        spaces
        pure $ Fields fs
  rs <- (precord <|> (SupType <$> pname)) `sepBy` string "<>"
  ds <- option [] $ do
    spaces *> string "deriving" <* spaces
    char '(' *> (pname `sepBy` char ',') <* char ')'
  endOfInput
  pure $ UnionRecord n rs ds

spaces = many space
safeN = letter <|> digit <|> choice (char <$> "_'.")
