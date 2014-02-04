{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TinyRoguelike.LevelParser
( str
, parseLevelDesc
, LevelDescription (..)
, parse_level_desc
, level
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

str = QuasiQuoter { quoteExp = stringE }

-- ================================================================== --
-- Parser for level data
{- Example level data
    # = (Stone, _, Brick, _)
    . = (Stone, _, _, _)
    @ = (Stone, _, _, Player)
    g = (Stone, _, _, Goblin)
    0 = (Stone, _, ClosedDoor, _)
    + = (Stone, _, OpenedDoor, _)
    ~ = (Lava, _, _, _)
    (80x20)
    ################################################################################
    #...............#..............................................................#
    #.....g.........#..............................................................#
    #...............#....g.........................................................#
    #...............#..............................................................#
    #...............0..............................................................#
    #################...........~~~~...............................................#
    #....g..........#.........~~~~~~~~.............................................#
    #............@..#.........~~~~~~~~.............................................#
    #...............#.g.........~~~~~~~~...........................................#
    #...............#.............~~~~~~~~.........................................#
    ###############+#.................~~~~.........................................#
    #..................................~~..........................................#
    #..............................................................................#
    #...g..........................................................................#
    #..............................................................................#
    #..............................................................................#
    #..............................................................................#
    #..............................................................................#
    ################################################################################
-}

type TileDescription = (Maybe String, Maybe String, Maybe String, Maybe String)

data LevelDescription = LevelDesc
    { table :: [(Char, TileDescription)]
    , dimension :: (Int, Int)
    , tiles :: [Char]
    } deriving (Typeable, Data)

parseLevelDesc :: String -> Either String LevelDescription
parseLevelDesc levelData =
    case runParser parse_level_desc () "" levelData of
      Left err  -> Left $ show err
      Right e   -> Right e
    {-where
        -- For now return an empty level
        parser = return $ LevelDesc
            { table = [('#', (Just "Stone", Nothing, Just "Brick", Nothing))
                      ,('.', (Just "Stone", Nothing, Nothing, Nothing))]
            , dimension = (80, 20)
            --, tiles = replicate (80*20) '.'
            , tiles = map
                        (\b -> if b then '#' else '.')
                        (take (80*20) $ randoms (mkStdGen 73837))
            }-}

whitespaces = many . oneOf $ " \t"

newlines = many . oneOf $ "\r\n"

parse_int :: Parser Int
parse_int = do
    str <- many digit
    return $ read str

parse_tile_symbol :: Parser Char
parse_tile_symbol = noneOf " \t" <?> "tile symbol"

parse_object_name :: Parser (Maybe String)
parse_object_name = do
    str <- many1 (noneOf " \t\r\n,)") <?> "object name"
    if str == "_"
        then return $ Nothing
        else return $ Just str


parse_tile_description :: Parser (Char, TileDescription)
parse_tile_description = do
    whitespaces
    ch <- parse_tile_symbol
    whitespaces >> char '=' >> whitespaces >> char '(' >> whitespaces
    mFloor <- parse_object_name
    whitespaces >> char ',' >> whitespaces
    mItem <- parse_object_name
    whitespaces >> char ',' >> whitespaces
    mWall <- parse_object_name
    whitespaces >> char ',' >> whitespaces
    mAvatar <- parse_object_name
    whitespaces >> char ')' >> whitespaces
    newlines
    return (ch, (mFloor, mItem, mWall, mAvatar))

parse_tile_lookup :: Parser [(Char, TileDescription)]
parse_tile_lookup = do
    list <- manyTill parse_tile_description (lookAhead $ try parse_level_dimension)
    return list

parse_level_dimension :: Parser (Int, Int)
parse_level_dimension = do
    whitespaces
    char '('
    whitespaces
    w <- parse_int
    whitespaces
    char 'x'
    whitespaces
    h <- parse_int
    whitespaces
    char ')'
    newlines
    return (w, h)

parse_tile_list :: Int -> Parser [Char]
parse_tile_list n = do
    --list <- manyTill (spaces >> parse_tile_symbol) (lookAhead . try $ (spaces >> eof))
    list <- count n (spaces >> parse_tile_symbol)
    return list

parse_level_desc :: Parser LevelDescription
parse_level_desc = do
    --many $ newline
    spaces
    tileLookup <- parse_tile_lookup
    dim@(w,h) <- parse_level_dimension <?> "level dimensions"
    tiles <- parse_tile_list (w*h)
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
                (flip setSourceName) file $
                (flip setSourceLine) line $
                (flip setSourceColumn) col $
                pos
            parse_level_desc

quoteLevelE str = do
    loc <- location
    let pos = (loc_filename loc,
                fst (loc_start loc),
                snd (loc_start loc))

    desc <- parseLevelForQuoter pos str
    dataToExpQ (const Nothing) desc


level :: QuasiQuoter
level = QuasiQuoter { quoteExp = quoteLevelE }

