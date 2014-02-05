{-# LANGUAGE DeriveDataTypeable #-}

module TinyRoguelike.LevelParser
( LevelDescription (..)
, parseLevelDescription
, levelDesc -- Quasiquoter
)
where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Language.Haskell.Meta.Parse

import Data.Generics
import Data.Char
import Data.List
import Data.Maybe
import Data.Grid
import Text.Parsec
import Text.Parsec.String
import System.Random
import Control.Monad
import Data.Data
import Data.Typeable

type TileDescription = (Maybe String, Maybe String, Maybe String, Maybe String)

data LevelDescription = LevelDesc
    { table :: [(Char, TileDescription)]
    , dimension :: (Int, Int)
    , tiles :: [Char]
    } deriving (Typeable, Data)

parseLevelDescription :: String -> Either String LevelDescription
parseLevelDescription levelData =
    case runParser parseLevelDesc () "" levelData of
      Left err  -> Left $ show err
      Right e   -> Right e

whitespaces = many . oneOf $ " \t"

newlines = many . oneOf $ "\r\n"

parseInt :: Parser Int
parseInt = do
    str <- many digit
    return $ read str

parseTileSymbol :: Parser Char
parseTileSymbol = noneOf " \t" <?> "tile symbol"

parseObjectName :: Parser (Maybe String)
parseObjectName = do
    str <- many1 (noneOf " \t\r\n,)") <?> "object name"
    return $ if str == "_" then Nothing else Just str


parseTileDescription :: Parser (Char, TileDescription)
parseTileDescription = do
    whitespaces
    ch <- parseTileSymbol
    whitespaces >> char '=' >> whitespaces >> char '(' >> whitespaces
    mFloor <- parseObjectName
    whitespaces >> char ',' >> whitespaces
    mItem <- parseObjectName
    whitespaces >> char ',' >> whitespaces
    mWall <- parseObjectName
    whitespaces >> char ',' >> whitespaces
    mAvatar <- parseObjectName
    whitespaces >> char ')' >> whitespaces
    newlines
    return (ch, (mFloor, mItem, mWall, mAvatar))

parseTileLookup :: Parser [(Char, TileDescription)]
parseTileLookup = manyTill parseTileDescription (lookAhead $ try parseLevelDimension)

parseLevelDimension :: Parser (Int, Int)
parseLevelDimension = do
    whitespaces
    char '('
    whitespaces
    w <- parseInt
    whitespaces
    char 'x'
    whitespaces
    h <- parseInt
    whitespaces
    char ')'
    newlines
    return (w, h)

parseTileList :: Int -> Parser [Char]
parseTileList n = count n (spaces >> parseTileSymbol)

parseLevelDesc :: Parser LevelDescription
parseLevelDesc = do
    --many $ newline
    spaces
    tileLookup <- parseTileLookup
    dim@(w,h) <- parseLevelDimension <?> "level dimensions"
    tiles <- parseTileList (w*h)
    spaces >> eof
    --when ((w*h) /= length tiles) $ do
    --    unexpected "Not enough tiles."
    return $ LevelDesc tileLookup dim tiles

-- The Quasi Quoter

parseLevelForQuoter :: Monad m => (String, Int, Int) -> String -> m LevelDescription
parseLevelForQuoter (file, line, col) str =
    case runParser p () "" str of
      Left err  -> fail $ show err
      Right e   -> return e
    where
        p = do
            pos <- getPosition
            setPosition $
                flip setSourceName file $
                flip setSourceLine line $
                setSourceColumn pos col
            parseLevelDesc

quoteLevelE str = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))

    desc <- parseLevelForQuoter pos str
    dataToExpQ (const Nothing) desc


levelDesc :: QuasiQuoter
levelDesc = QuasiQuoter { quoteExp = quoteLevelE }

