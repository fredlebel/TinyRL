{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

module TinyRoguelike.LevelParser
( str
, parseLevel
, LevelDescription (..)
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
    }

parseLevel :: String -> Either String LevelDescription
parseLevel levelData =
    case runParser parse_level () "" levelData of
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

parse_int :: Parser Int
parse_int = do
    str <- many digit
    return $ read str


parse_tile_symbol :: Parser Char
parse_tile_symbol = noneOf " \t"

parse_object_name :: Parser (Maybe String)
parse_object_name = do
    str <- many (noneOf " \t,)")
    if str == "_"
        then return $ Nothing
        else return $ Just str


parse_tile_description :: Parser (Char, TileDescription)
parse_tile_description = do
    spaces
    ch <- parse_tile_symbol
    spaces >> char '=' >> spaces >> char '(' >> spaces
    mFloor <- parse_object_name
    spaces >> char ',' >> spaces
    mItem <- parse_object_name
    spaces >> char ',' >> spaces
    mWall <- parse_object_name
    spaces >> char ',' >> spaces
    mAvatar <- parse_object_name
    spaces >> char ')' >> spaces
    return (ch, (mFloor, mItem, mWall, mAvatar))

parse_tile_lookup :: Parser [(Char, TileDescription)]
parse_tile_lookup = do
    list <- manyTill parse_tile_description (lookAhead $ try parse_level_dimension)
    return list

parse_level_dimension :: Parser (Int, Int)
parse_level_dimension = do
    char '('
    w <- parse_int
    char 'x'
    h <- parse_int
    char ')'
    return (w, h)

parse_tile_list :: Parser [Char]
parse_tile_list = do
    list <- manyTill (spaces >> parse_tile_symbol) (lookAhead . try $ (spaces >> eof))
    return list

parse_level :: Parser LevelDescription
parse_level = do
    many $ newline
    tileLookup <- parse_tile_lookup
    spaces
    dim <- parse_level_dimension
    spaces
    tiles <- parse_tile_list
    return $ LevelDesc tileLookup dim tiles


