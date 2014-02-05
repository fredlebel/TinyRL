{-# LANGUAGE DeriveDataTypeable #-}

module TinyRoguelike.LevelParser
( LevelDescription (..)
, parseLevelDescription
, levelDesc -- Quasiquoter
)
where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Generics
import Text.Parsec
import Text.Parsec.String
import Control.Monad


type TileDescription = (Maybe String, Maybe String, Maybe String, Maybe String)

type TileChar = Char

data LevelDescription = LevelDesc
    { table :: [(Char, TileDescription)]
    , dimension :: (Int, Int)
    , tiles :: [TileChar]
    } deriving (Typeable, Data)

parseLevelDescription :: String -> Either String LevelDescription
parseLevelDescription levelData =
    case runParser parseLevelDesc () "" levelData of
      Left err  -> Left $ show err
      Right e   -> Right e

-- Parsing helpers
whitespaces :: Parser ()
whitespaces = void $ many . oneOf $ " \t"
paddedChar :: Char -> Parser ()
paddedChar ch = void $ whitespaces >> char ch >> whitespaces
newlines :: Parser ()
newlines = void . many . oneOf $ "\r\n"

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
    paddedChar '=' >> paddedChar '('
    mFloor <- parseObjectName
    paddedChar ','
    mItem <- parseObjectName
    paddedChar ','
    mWall <- parseObjectName
    paddedChar ','
    mAvatar <- parseObjectName
    paddedChar ')'
    newlines
    return (ch, (mFloor, mItem, mWall, mAvatar))

parseTileLookup :: Parser [(Char, TileDescription)]
parseTileLookup = manyTill parseTileDescription (lookAhead $ try parseLevelDimension)

parseLevelDimension :: Parser (Int, Int)
parseLevelDimension = do
    paddedChar '('
    w <- parseInt
    paddedChar 'x'
    h <- parseInt
    paddedChar ')'
    return (w, h)

parseTileList :: Int -> Parser [TileChar]
parseTileList n = count n (spaces >> parseTileSymbol)

parseLevelDesc :: Parser LevelDescription
parseLevelDesc = do
    --many $ newline
    spaces
    tileLookup <- parseTileLookup
    dim@(w,h) <- parseLevelDimension <?> "level dimensions"
    tilesChars <- parseTileList (w*h)
    spaces >> eof
    --when ((w*h) /= length tiles) $ do
    --    unexpected "Not enough tiles."
    return $ LevelDesc tileLookup dim tilesChars

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

quoteLevelE :: String -> Q Exp
quoteLevelE str = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))

    desc <- parseLevelForQuoter pos str
    dataToExpQ (const Nothing) desc


levelDesc :: QuasiQuoter
levelDesc = QuasiQuoter
    { quoteExp = quoteLevelE
    , quotePat = fail
    , quoteType = fail
    , quoteDec = fail
    }

