{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Record.Extend
  ( extendQQ
  , extendD
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Attoparsec.Text      as P
import           Data.Char
import           Data.Maybe
import           Data.Text                 (pack)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Prelude                   hiding (takeWhile)

extendQQ = QuasiQuoter
  { quoteDec = extendD
  , quoteExp = error "only quoteDec is exist"
  , quotePat = error "only quoteDec is exist"
  , quoteType = error "only quoteDec is exist"
  }

extendD :: String -> Q [Dec]
extendD input =  do
  let s = either error id $ parseOnly unionRecordParser $ pack input
  ss <- mapM mkRecordElem (elems s)
  derivs' <- DerivClause Nothing <$> mapM mkDerive (derivs s)
  name' <- newName (name s)
  rname' <- newName (name s)
  pure [DataD (ss >>= fst3) name' (ss >>= snd3) Nothing [RecC rname' (ss >>= trd3)] [derivs']]
  where
    mkRecordElem (SupType r) = do
      sub <- lookupType' r
      TyConI (DataD ctx _ bndrs _ (RecC _ rtys:_) _) <- reify sub
      pure (ctx, bndrs, rtys)
    mkRecordElem (Fields fs) = do
      rtys <- forM fs $ \f -> do
        let n = mkName (fst f)
        t <- lookupType' (snd f)
        pure (n, Bang NoSourceUnpackedness NoSourceStrictness, ConT t) -- todo: impl bang
      pure ([], [], rtys)
    mkDerive d = ConT <$> lookupType' d

lookupType' :: String -> Q Name
lookupType' tn = lookupTypeName tn >>= maybe (error $ "not in scope type " <> tn) pure

fst3 :: (a, b, c) -> a
fst3 (x,_,_) = x

snd3 :: (a, b, c) -> b
snd3 (_,x,_) = x

trd3 :: (a, b, c) -> c
trd3 (_,_,x) = x

data UnionRecord = UnionRecord
  { name   :: String
  , elems  :: [Element]
  , derivs :: [String]
  } deriving (Show, Eq)

data Element = SupType String | Fields [(String, String)]
  deriving (Show, Eq)

unionRecordParser :: Parser UnionRecord
unionRecordParser = do
  spaces *> string "data"
  spaces1
  n <- many1 safeN
  spaces
  char '='
  let p_name = spaces *> many1 safeN <* spaces
  let p_singleField = do
        n <- p_name
        string "::"
        t <- p_name
        pure (n, t)
  let precord = do
        spaces *> char '{'
        fs <- p_singleField `sepBy` char ','
        spaces *> char '}'
        spaces
        pure $ Fields fs
  rs <- (precord <|> (SupType <$> p_name)) `sepBy` string "<>"
  ds <- option [] $ do
    spaces *> string "deriving" <* spaces
    char '(' *> (p_name `sepBy` char ',') <* char ')'
  spaces *> endOfInput
  pure $ UnionRecord n rs ds

spaces :: Parser String
spaces = many space

spaces1 :: Parser String
spaces1 = many1 space

safeN :: Parser Char
safeN = letter <|> digit <|> choice (char <$> "_'.")
